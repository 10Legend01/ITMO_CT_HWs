from gen.Grm2Parser import Grm2Parser
from gen.Grm2Visitor import Grm2Visitor


from generator.Term_NonTerm import Term, NonTerm

import re


class MyVisitor(Grm2Visitor):

    def __init__(self):
        self._potential_terms = set()
        self._potential_non_terms = set()
        self._names_terms = set()

        self._terms = list()
        self._non_terms = dict()
        self._S = None

    _clear = __init__

    def get_ll1_data(self, tree):
        assert isinstance(tree, Grm2Parser.StartContext), "Начало не в start."
        self._clear()
        self.visit(tree)
        self._check_terms_and_non_terms()

        # https://neerc.ifmo.ru/wiki/index.php?title=%D0%A4%D0%BE%D1%80%D0%BC%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D0%B5_%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B0%D1%82%D0%B8%D0%BA%D0%B8
        # Γ=⟨Σ,N,S,P⟩
        # Набор правил P внутри нетерминалов.
        # return Σ,(N,P),S
        return self._terms, self._non_terms, self._S

    def _check_terms_and_non_terms(self):
        assert self._potential_terms - self._names_terms == set(), \
            "Найдены терминалы, которых нет в грамматике: {}".format(
                self._potential_terms - self._names_terms
            )
        assert self._potential_non_terms - self._non_terms.keys() == set(), \
            "Найдены нетерминалы, которых нет в грамматике: {}".format(
                self._potential_non_terms - self._non_terms.keys()
            )
        assert self._S in self._non_terms.keys(), "Не найден стартовый терминал с указанным именем"

    def visitStart(self, ctx: Grm2Parser.StartContext):
        self._S = ctx.NONTERM().getText()
        self.visitChildren(ctx)

    def visitChoose_rule(self, ctx: Grm2Parser.Choose_ruleContext):
        if ctx.TERM():
            name = ctx.TERM().getText()
            assert name not in self._names_terms, "Два терминала с одинаковым именем"
            self._names_terms.add(name)
            # expr = ctx.STRING().getText()
            expr = ctx.STRING().getText()[1:-1].replace("\\\'", "\'")
            try:
                assert re.findall(expr, "") == [], "Терминал {} допускает пустую строку".format(name)
            except re.error:
                raise SyntaxError("Недопустимый шаблон регулярного выражения в терминале: {}".format(name))
            if ctx.skip_rule():
                skip = True
            else:
                skip = False
            self._terms.append(Term(name, expr, skip))
        else:
            self.visitChildren(ctx)

    def visitNon_term_rule(self, ctx: Grm2Parser.Non_term_ruleContext):
        name = ctx.NONTERM().getText()
        assert name not in self._non_terms.keys(), "Два нетерминала с одинаковым именем"
        args = self.visitArgs(ctx.args()) if ctx.args() else []
        returns = self.visitNon_term_returns(ctx.non_term_returns()) if ctx.non_term_returns() else []
        rules = []

        for i in ctx.rightPart():
            rules.append(self.visitRightPart(i))
        self._non_terms[name] = NonTerm(name, args, returns, rules)

    def visitArgs(self, ctx: Grm2Parser.ArgsContext):
        return [i.getText() for i in ctx.arg()]

    def visitNon_term_returns(self, ctx: Grm2Parser.Non_term_returnsContext):
        return self.visitArgs(ctx.args())

    def visitRightPart(self, ctx: Grm2Parser.RightPartContext):
        if not ctx.ruleToken():
            return []
        ma = list()
        for child in ctx.children:
            ma.append(self.visitRuleToken(child))
        return ma

    def visitRuleToken(self, ctx: Grm2Parser.RuleTokenContext):
        def parse_code(s: str) -> str:
            return s[(1 if s[0] == '{' else None):(-1 if s[-1] == '}' else None)]\
                .strip().replace("\\{", "{").replace("\\}", "}")

        if ctx.TERM():
            self._potential_terms.add(ctx.TERM().getText())
            return NonTerm.TERM, ctx.TERM().getText()
        elif ctx.NONTERM():
            self._potential_non_terms.add(ctx.NONTERM().getText())
            # return NonTerm.NONTERM, ctx.NONTERM().getText(), *[i.getText() for i in ctx.param()]
            return NonTerm.NONTERM, ctx.NONTERM().getText(), *[parse_code(i.getText()) for i in ctx.param()]
        elif ctx.CODE():
            # return self.NonTerm.CODE, ctx.CODE().getText()
            # return NonTerm.CODE, ctx.CODE().getText()[1:-1].strip().replace("\\{", "{").replace("\\}", "}")
            return NonTerm.CODE, parse_code(ctx.CODE().getText())
