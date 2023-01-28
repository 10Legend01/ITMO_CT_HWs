from .LexicalAnalyzer import *
from .Tree import *


class SyntaxAnalyzer:

    def __init__(self, lex: LexicalAnalyzer = None, s: str = None):
        if lex is None:
            if s is None:
                raise "Need 'LexicalAnalyzer' or 'str' type to input"
            lex = LexicalAnalyzer(s)
        self.lex = lex
        assert self.lex.cur_token() is None, "Lexical analyzer has already been used"
        self.lex.next_token()

    def get_tree(self) -> Tree:
        tree = self._b()
        if self.lex.cur_token() != Token.END:
            raise SyntaxError(f"End of statement expected, found {self.lex.cur_token()}")
        return tree

    def _b(self) -> Tree:
        res = Tree("B")
        if self.lex.cur_token() in [Token.VAR, Token.LPAREN, Token.NOT]:
            res.add_child(self._e())
            res.add_child(self._b_prime())
        else:
            raise SyntaxError("Expected: var, open bracket, not")
        return res

    def _b_prime(self) -> Tree:
        res = Tree("B'")
        if self.lex.cur_token() in [Token.XOR]:
            res.add_child(Tree("xor"))
            self.lex.next_token()
            res.add_child(self._e())
            res.add_child(self._b_prime())
        elif self.lex.cur_token() in [Token.END, Token.RPAREN]:
            pass
        else:
            raise SyntaxError("Expected: xor, close bracket, end")
        return res

    def _e(self) -> Tree:
        return Tree("E", [self._t(), self._e_prime()])

    def _e_prime(self) -> Tree:
        res = Tree("E'")
        if self.lex.cur_token() in [Token.OR]:
            res.add_child(Tree("or"))
            self.lex.next_token()
            res.add_child(self._t())
            res.add_child(self._e_prime())
        elif self.lex.cur_token() in [Token.END, Token.RPAREN, Token.XOR]:
            pass
        else:
            raise SyntaxError("Expected: or, xor, close bracket, end")
        return res

    def _t(self) -> Tree:
        return Tree("T", [self._p(), self._t_prime()])

    def _t_prime(self) -> Tree:
        res = Tree("T'")
        if self.lex.cur_token() in [Token.AND]:
            res.add_child(Tree("and"))
            self.lex.next_token()
            res.add_child(self._p())
            res.add_child(self._t_prime())
        elif self.lex.cur_token() in [Token.END, Token.RPAREN, Token.XOR, Token.OR]:
            pass
        else:
            raise SyntaxError("Expected: and, or, xor, close bracket, end")
        return res

    def _p(self) -> Tree:
        res = Tree("P")
        if self.lex.cur_token() == Token.NOT:
            res.add_child(Tree("not"))
            self.lex.next_token()
            res.add_child(self._p())
        elif self.lex.cur_token() in [Token.VAR, Token.LPAREN]:
            res.add_child(self._v())
            res.add_child(self._p_prime())
        else:
            raise SyntaxError("Expected: var, open bracket, not")
        return res

    def _p_prime(self) -> Tree:
        res = Tree("P'")
        if self.lex.cur_token() == Token.IN:
            res.add_child(Tree("in"))
            self.lex.next_token()
            res.add_child(self._v())
        elif self.lex.cur_token() == Token.NOT:
            res.add_child(Tree("not"))
            self.lex.next_token()
            if self.lex.cur_token() == Token.IN:
                res.add_child(Tree("in"))
                self.lex.next_token()
                res.add_child(self._v())
            else:
                raise SyntaxError("Expected: in")
        elif self.lex.cur_token() in [Token.END, Token.RPAREN, Token.XOR, Token.OR, Token.AND]:
            pass
        else:
            raise SyntaxError("Expected: or, xor, close bracket, end")
        return res

    def _v(self) -> Tree:
        res = Tree("V")
        if self.lex.cur_token() == Token.LPAREN:
            res.add_child(Tree("("))
            self.lex.next_token()
            res.add_child(self._b())
            if self.lex.cur_token() == Token.RPAREN:
                res.add_child(Tree(")"))
                self.lex.next_token()
            else:
                raise SyntaxError("Expected: close bracket")
        elif self.lex.cur_token() == Token.VAR:
            res.add_child(Tree(f"Var {self.lex.get_message()}"))
            self.lex.next_token()
        else:
            raise SyntaxError("Expected: var, open bracket, not")
        return res
