if __name__ is not None and "." in __name__:
    from .Lab2Lexical import *
    from .Lab2Token import *
else:
    from Lab2Lexical import *
    from Lab2Token import *


class Universal:
    def __init__(self, text="", token=0):
        self.text = text
        if text:
            self.token = token
        else:
            self.children = []

    def add_child(self, child, text=""):
        self.children.append(child)
        if isinstance(child, int):
            self.text += text
            return self.temp(text, child)
        else:
            self.text += child.text
            return child

    @classmethod
    def temp(cls, text, token):
        return cls(text, token)


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
        tree = self.b()
        if self.lex.cur_token() != Token.end:
            raise SyntaxError(f"End of statement expected, found {self.lex.cur_token()}")
        return tree

    class B(Universal):
        def __init__(self):
            super().__init__()
            self.val = None

    def b(self) -> B:
        res = self.B()
        if self.lex.cur_token() in [Token.VAR, Token.LPAR, Token.NOT]:
            _e = res.add_child(self._e())
            res.add_child(self._b_prime())
        else:
            raise SyntaxError("Expected: var, open bracket, not")
        return res

    class B_prime(Universal):
        def __init__(self):
            super().__init__()

    def _b_prime(self) -> B_prime:
        res = self.B_prime()
        if self.lex.cur_token() in [Token.XOR]:
            XOR = res.add_child(Token.XOR, self.lex.get_text())
            self.lex.next_token()
            res.add_child(self._e())
            res.add_child(self._b_prime())
        return res

    def _e(self):
        return Tree("E", [self._t(), self._e_prime()])

    def _e_prime(self):
        res = Tree("E'")
        if self.lex.cur_token() in [Token.OR]:
            res.add_child(Tree("or"))
            self.lex.next_token()
            res.add_child(self._t())
            res.add_child(self._e_prime())
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
        return res

    def _p(self) -> Tree:
        res = Tree("P")
        if self.lex.cur_token() == Token.NOT:
            res.add_child(Tree("not"))
            self.lex.next_token()
            res.add_child(self._p_prime_prime())
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
        return res

    def _p_prime_prime(self) -> Tree:
        res = Tree("P''")
        if self.lex.cur_token() == Token.NOT:
            res.add_child(Tree("not"))
            self.lex.next_token()
            res.add_child(self._p_prime_prime())
        elif self.lex.cur_token() in [Token.VAR, Token.LPAREN]:
            res.add_child(self._v())
            res.add_child(self._p_prime_prime_prime())
        else:
            raise SyntaxError("Expected: var, open bracket, not")
        return res

    def _p_prime_prime_prime(self) -> Tree:
        res = Tree("P'''")
        if self.lex.cur_token() == Token.IN:
            res.add_child(Tree("in"))
            self.lex.next_token()
            res.add_child(self._v())
        return res

    def _v(self) -> Tree:
        res = Tree("V")
        if self.lex.cur_token() == Token.LPAREN:
            res.add_child(Tree("("))
            self.lex.next_token()
            res.add_child(self.b())
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
