from .Term_NonTerm import *

import os

class Generator:
    _tab = " " * 4

    def __init__(self, name, path):
        if path:
            path += '/'

        self.name = name
        self.path = path

        self._filename_token = name + "Token"
        self._filename_lexical = name + "Lexical"
        self._filename_syntax = name + "Syntax"

    def greate_all(self, _terms, _S, _non_terms, _tokens_distributor, _eps):
        self.greate_tokens_file(_terms)
        self.greate_lexical_file()
        self.greate_syntax_file(_S, _non_terms, _tokens_distributor, _eps)

    def _write(self, text, filename):
        if not os.path.exists(self.path):
            os.makedirs(self.path)
            with open(self.path + '__init__.py', 'w') as _:
                pass

        with open(self.path + filename + '.py', 'w') as file:
            file.write(text)

    def greate_tokens_file(self, _terms):

        def _get_texts():
            text_token = ""
            text_token_template = "\n{}{} = {}"
            text_expr = ""
            text_expr_template = "\n{}(\'{}\', \'{}\'),"
            skips = set()

            i = 0
            for term in _terms:
                term: Term
                text_token += text_token_template.format(self._tab, term.name, i)
                text_expr += text_expr_template.format(self._tab, term.name, term.expr)
                if term.skip:
                    skips.add(term.name)
                i += 1

            name = "_MISMATCH_"
            expr = "."
            text_expr += text_expr_template.format(self._tab, name, expr)

            name = "end"
            text_token += text_token_template.format(self._tab, name, i)
            i += 1

            return text_token, text_expr, str(skips)

        text = \
            """from enum import Enum


class Token(Enum):{}


token_specification = [{}
]

skip_tokens = {}
""".format(*_get_texts())

        self._write(text, self._filename_token)

    def greate_lexical_file(self):
        text = \
            rf"""if __name__ is not None and "." in __name__:
    from .{self._filename_token} import *
else:
    from {self._filename_token} import *
""" + r"""

import re


class ParseException(Exception):
    def __init__(self, data):
        self.data = data

    def __str__(self):
        return repr(self.data)


class LexicalAnalyzer:
    _save = (None, None)
    _generator = None

    def __init__(self, s: str):
        self._generator = self._token_gen(s)

    @staticmethod
    def _token_gen(s: str):
        tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)
        for mo in re.finditer(tok_regex, s):
            if mo.lastgroup == '_MISMATCH_':
                raise ParseException(f"Can't parse on pos: '{mo.span()}'")
            elif mo.lastgroup in skip_tokens:
                continue
            yield Token.__dict__[mo.lastgroup], mo.group(), mo.span()
        yield Token.end, "", (len(s), len(s) + 1)

    def next_token(self):
        self._save = next(self._generator)
        return self.cur_token()

    def cur_token(self):
        return self._save[0]

    def get_text(self):
        return self._save[1]

    def get_pos(self):
        return self._save[2]
"""
        self._write(text, self._filename_lexical)

    def greate_syntax_file(self, _S: str, _non_terms, _tokens_distributor, _eps):

        def first_upper(s: str) -> str:
            return s[0].upper() + s[1:]

        def generate_class(non_term: NonTerm) -> str:
            _text = \
                """

    class {}(Universal):
        def __init__(self):
            super().__init__()
            {}
""".format(first_upper(non_term.name), f"{3 * self._tab}".join(map(lambda x: f"self.{x} = None\n", non_term.returns)))

            return _text

        def add_rule(rule: Tuple[int, str]) -> str:
            _text = ""

            for part in rule:
                if part[0] == NonTerm.TERM:
                    _text += "{}if self.lex.cur_token() != Token.{}:\n{}raise SyntaxError(\"Expected: {}; on pos: \" + str(self.lex.get_pos()))\n".format(
                        3 * self._tab, part[1],
                        4 * self._tab, part[1]
                    )
                    _text += 3 * self._tab + "{} = res.add_child(Token.{}, self.lex.get_text())\n".format(
                        part[1], part[1]
                    )
                    _text += 3 * self._tab + "self.lex.next_token()\n"

                elif part[0] == NonTerm.NONTERM:
                    _text += 3 * self._tab + "{} = res.add_child(self.{}({}))\n".format(
                        part[1], part[1], ", ".join(part[2:])
                    )
                elif part[0] == NonTerm.CODE:
                    # _text += 3 * self._tab + "exec(\"{}\")\n".format(part[1])
                    _text += 3 * self._tab + part[1] + "\n"

            return _text if _text else 3 * self._tab + "pass\n"

        def generate_def(non_term: NonTerm) -> str:
            _text = "{}def {}(self{}) -> {}:\n{}res = self.{}()\n".format(
                1 * self._tab, non_term.name, ", " + ", ".join(non_term.args) if non_term.args else "",
                first_upper(non_term.name),
                2 * self._tab, first_upper(non_term.name),
            )
            for ret in non_term.returns:
                _text += 2 * self._tab + f"{ret} = None\n"

            if_flag = True
            for i in range(len(non_term.rules)):
                if _eps[non_term.name] == i:
                    continue
                _text += 2 * self._tab + ("if" if if_flag else "elif") + " self.lex.cur_token() in {}:\n".format(
                    "{" + ", ".join(map(lambda x: f"Token.{x}", _tokens_distributor[non_term.name][i])) + "}"
                )
                _text += add_rule(non_term.rules[i])
                if_flag = False

            _text += 2 * self._tab + f"else:\n"
            if _eps[non_term.name] != -1:
                _text += add_rule(non_term.rules[_eps[non_term.name]])
            else:
                _text += 3 * self._tab + "raise SyntaxError(\"Expected: {}; on pos: \" + str(self.lex.get_pos()))\n".format(
                    str(set.union(*_tokens_distributor[non_term.name])),

                )

            for ret in non_term.returns:
                _text += 2 * self._tab + f"res.{ret} = {ret}\n"
            _text += 2 * self._tab + "return res"
            return _text

        def generate_syntax():
            _text = ""
            for non_term in _non_terms.values():
                _text += generate_class(non_term) + generate_def(non_term)
            return _text

        text = \
            r"""if __name__ is not None and "." in __name__:
    from .{} import *
    from .{} import *
else:
    from {} import *
    from {} import *


class Universal:
    def __init__(self, text="", token=None):
        if text:
            self.text = text
            self.token = token
        else:
            self.children = []

    def add_child(self, child, text=""):
        if isinstance(child, Token):
            child = Universal(text=text, token=child)
        self.children.append(child)
        return child

    def __str__(self):
        return str(self.__class__) + \
               ("(" + ", ".join(map(str, self.children)) + ")") if "children" in self.__dict__ else ""

    def __repr__(self):
        return str(self)


class SyntaxAnalyzer:

    def __init__(self, lex: LexicalAnalyzer = None, s: str = None):
        if lex is None:
            if s is None:
                raise "Need 'LexicalAnalyzer' or 'str' type to input"
            lex = LexicalAnalyzer(s)
        self.lex = lex
        assert self.lex.cur_token() is None, "Lexical analyzer has already been used"
        self.lex.next_token()

    def get_tree(self):
        tree = self.{}()
        if self.lex.cur_token() != Token.end:
            raise SyntaxError("End of statement expected, found %s on pos %s" 
                              % self.lex.cur_token(), self.lex.get_pos()
                              )
        return tree{}
""".format(*(self._filename_token, self._filename_lexical) * 2, _S, generate_syntax())

        self._write(text, self._filename_syntax)
