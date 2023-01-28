from antlr4 import *
from .MyVisitor import *
from .Generator import *
from gen.Grm2Lexer import Grm2Lexer


class Analyzer:

    def __init__(self, file: str, name: str, path="", generate_code=True):

        self._terms = None
        self._non_terms = None
        self._S = None

        self.stack = []

        self._tokens_distributor = dict()  # { nonterm: [tokens1 -> set(), tokens2 -> set(), ...] }
        self._first = dict()  # FIRST = { nonterm: tokens1 + tokens2 + ... -> set() }
        self._eps = dict()  # { nonterm: EPS in _tokens_union -> int } ; '-1' if haven't EPS, 'i' if have EPS on rule[i]

        self.create(file)

        if generate_code:
            generator = Generator(name, path)
            generator.greate_all(self._terms, self._S, self._non_terms, self._tokens_distributor, self._eps)

    def create(self, file):
        input_stream = FileStream(file)
        lexer = Grm2Lexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = Grm2Parser(stream)
        tree = parser.start()

        visitor = MyVisitor()
        # data = visitor.get_LL1_data(tree)
        self._terms, self._non_terms, self._S = \
            visitor.get_ll1_data(tree)

        self.filler()

        print(self._tokens_distributor)
        print(self._first)
        print(self._eps)

    def filler(self):
        self.stack = []
        for non_term in self._non_terms.values():
            self._dfs(non_term)

    def _dfs(self, non_term: NonTerm):
        if non_term.name in self._first.keys():
            return self._first[non_term.name]

        assert non_term.name not in self.stack, "Обнаружена рекурсия"
        self.stack.append(non_term.name)

        self._tokens_distributor[non_term.name] = [set() for _ in non_term.rules]
        self._first[non_term.name] = set()
        self._eps[non_term.name] = -1

        k = 0
        for rule in non_term.rules:

            _tokens = set()

            for i in rule:
                if i[0] == NonTerm.NONTERM:
                    get_tokens = self._dfs(self._non_terms[i[1]])
                    _tokens.update(get_tokens)
                    if self._eps[i[1]] != -1:
                        continue
                    break
                elif i[0] == NonTerm.TERM:
                    _tokens.add(i[1])
                    break
            else:
                assert self._eps[non_term.name] == -1, \
                    "Обнаружены два правила с EPS в нетерминале {}".format(non_term.name)
                self._eps[non_term.name] = k

            assert _tokens & self._first[non_term.name] == set(), "Нарушение правила LL(1)"
            self._first[non_term.name].update(_tokens)
            self._tokens_distributor[non_term.name][k].update(_tokens)

            k += 1

        self.stack.pop()
        return self._first[non_term.name]

