if __name__ is not None and "." in __name__:
    from .Lab2Token import *
    from .Lab2Lexical import *
else:
    from Lab2Token import *
    from Lab2Lexical import *


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
        tree = self.b()
        if self.lex.cur_token() != Token.end:
            raise SyntaxError("End of statement expected, found %s" % self.lex.cur_token())
        return tree

    class B(Universal):
        def __init__(self):
            super().__init__()
            
    def b(self) -> B:
        res = self.B()
        if self.lex.cur_token() in {Token.NOT, Token.LPAR, Token.VAR}:
            e = res.add_child(self.e())
            bP = res.add_child(self.bP())
        else:
            raise SyntaxError("Expected: {'NOT', 'LPAR', 'VAR'}")
        return res

    class BP(Universal):
        def __init__(self):
            super().__init__()
            
    def bP(self) -> BP:
        res = self.BP()
        if self.lex.cur_token() in {Token.XOR}:
            if self.lex.cur_token() != Token.XOR:
                raise SyntaxError("Expected: XOR")
            XOR = res.add_child(Token.XOR, self.lex.get_text())
            self.lex.next_token()
            e = res.add_child(self.e())
            bP = res.add_child(self.bP())
        else:
            pass
        return res

    class E(Universal):
        def __init__(self):
            super().__init__()
            
    def e(self) -> E:
        res = self.E()
        if self.lex.cur_token() in {Token.NOT, Token.LPAR, Token.VAR}:
            t = res.add_child(self.t())
            eP = res.add_child(self.eP())
        else:
            raise SyntaxError("Expected: {'NOT', 'LPAR', 'VAR'}")
        return res

    class EP(Universal):
        def __init__(self):
            super().__init__()
            
    def eP(self) -> EP:
        res = self.EP()
        if self.lex.cur_token() in {Token.OR}:
            if self.lex.cur_token() != Token.OR:
                raise SyntaxError("Expected: OR")
            OR = res.add_child(Token.OR, self.lex.get_text())
            self.lex.next_token()
            t = res.add_child(self.t())
            eP = res.add_child(self.eP())
        else:
            pass
        return res

    class T(Universal):
        def __init__(self):
            super().__init__()
            
    def t(self) -> T:
        res = self.T()
        if self.lex.cur_token() in {Token.NOT, Token.LPAR, Token.VAR}:
            p = res.add_child(self.p())
            tP = res.add_child(self.tP())
        else:
            raise SyntaxError("Expected: {'NOT', 'LPAR', 'VAR'}")
        return res

    class TP(Universal):
        def __init__(self):
            super().__init__()
            
    def tP(self) -> TP:
        res = self.TP()
        if self.lex.cur_token() in {Token.AND}:
            if self.lex.cur_token() != Token.AND:
                raise SyntaxError("Expected: AND")
            AND = res.add_child(Token.AND, self.lex.get_text())
            self.lex.next_token()
            p = res.add_child(self.p())
            tP = res.add_child(self.tP())
        else:
            pass
        return res

    class P(Universal):
        def __init__(self):
            super().__init__()
            
    def p(self) -> P:
        res = self.P()
        if self.lex.cur_token() in {Token.NOT}:
            if self.lex.cur_token() != Token.NOT:
                raise SyntaxError("Expected: NOT")
            NOT = res.add_child(Token.NOT, self.lex.get_text())
            self.lex.next_token()
            pPP = res.add_child(self.pPP())
        elif self.lex.cur_token() in {Token.LPAR, Token.VAR}:
            v = res.add_child(self.v())
            pP = res.add_child(self.pP())
        else:
            raise SyntaxError("Expected: {'NOT', 'LPAR', 'VAR'}")
        return res

    class PP(Universal):
        def __init__(self):
            super().__init__()
            
    def pP(self) -> PP:
        res = self.PP()
        if self.lex.cur_token() in {Token.IN}:
            if self.lex.cur_token() != Token.IN:
                raise SyntaxError("Expected: IN")
            IN = res.add_child(Token.IN, self.lex.get_text())
            self.lex.next_token()
            v = res.add_child(self.v())
        elif self.lex.cur_token() in {Token.NOT}:
            if self.lex.cur_token() != Token.NOT:
                raise SyntaxError("Expected: NOT")
            NOT = res.add_child(Token.NOT, self.lex.get_text())
            self.lex.next_token()
            if self.lex.cur_token() != Token.IN:
                raise SyntaxError("Expected: IN")
            IN = res.add_child(Token.IN, self.lex.get_text())
            self.lex.next_token()
            v = res.add_child(self.v())
        else:
            pass
        return res

    class PPP(Universal):
        def __init__(self):
            super().__init__()
            
    def pPP(self) -> PPP:
        res = self.PPP()
        if self.lex.cur_token() in {Token.NOT}:
            if self.lex.cur_token() != Token.NOT:
                raise SyntaxError("Expected: NOT")
            NOT = res.add_child(Token.NOT, self.lex.get_text())
            self.lex.next_token()
            pPP = res.add_child(self.pPP())
        elif self.lex.cur_token() in {Token.LPAR, Token.VAR}:
            v = res.add_child(self.v())
            pPPP = res.add_child(self.pPPP())
        else:
            raise SyntaxError("Expected: {'NOT', 'LPAR', 'VAR'}")
        return res

    class PPPP(Universal):
        def __init__(self):
            super().__init__()
            
    def pPPP(self) -> PPPP:
        res = self.PPPP()
        if self.lex.cur_token() in {Token.IN}:
            if self.lex.cur_token() != Token.IN:
                raise SyntaxError("Expected: IN")
            IN = res.add_child(Token.IN, self.lex.get_text())
            self.lex.next_token()
            v = res.add_child(self.v())
        else:
            pass
        return res

    class V(Universal):
        def __init__(self):
            super().__init__()
            
    def v(self) -> V:
        res = self.V()
        if self.lex.cur_token() in {Token.LPAR}:
            if self.lex.cur_token() != Token.LPAR:
                raise SyntaxError("Expected: LPAR")
            LPAR = res.add_child(Token.LPAR, self.lex.get_text())
            self.lex.next_token()
            b = res.add_child(self.b())
            if self.lex.cur_token() != Token.RPAR:
                raise SyntaxError("Expected: RPAR")
            RPAR = res.add_child(Token.RPAR, self.lex.get_text())
            self.lex.next_token()
        elif self.lex.cur_token() in {Token.VAR}:
            if self.lex.cur_token() != Token.VAR:
                raise SyntaxError("Expected: VAR")
            VAR = res.add_child(Token.VAR, self.lex.get_text())
            self.lex.next_token()
        else:
            raise SyntaxError("Expected: {'LPAR', 'VAR'}")
        return res
