if __name__ is not None and "." in __name__:
    from .CalcFactorialToken import *
    from .CalcFactorialLexical import *
else:
    from CalcFactorialToken import *
    from CalcFactorialLexical import *


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
        tree = self.expr()
        if self.lex.cur_token() != Token.end:
            raise SyntaxError("End of statement expected, found %s on pos %s" 
                              % self.lex.cur_token(), self.lex.get_pos()
                              )
        return tree

    class Expr(Universal):
        def __init__(self):
            super().__init__()
            self.val = None

    def expr(self) -> Expr:
        res = self.Expr()
        val = None
        if self.lex.cur_token() in {Token.SUB, Token.NUM, Token.LPAREN}:
            addSub = res.add_child(self.addSub())
            val = addSub.val
        else:
            raise SyntaxError("Expected: {'SUB', 'NUM', 'LPAREN'}; on pos: " + str(self.lex.get_pos()))
        res.val = val
        return res

    class AddSub(Universal):
        def __init__(self):
            super().__init__()
            self.val = None

    def addSub(self) -> AddSub:
        res = self.AddSub()
        val = None
        if self.lex.cur_token() in {Token.SUB, Token.NUM, Token.LPAREN}:
            mulDiv = res.add_child(self.mulDiv())
            addSubP = res.add_child(self.addSubP(mulDiv.val))
            val = addSubP.val
        else:
            raise SyntaxError("Expected: {'SUB', 'NUM', 'LPAREN'}; on pos: " + str(self.lex.get_pos()))
        res.val = val
        return res

    class AddSubP(Universal):
        def __init__(self):
            super().__init__()
            self.val = None

    def addSubP(self, left) -> AddSubP:
        res = self.AddSubP()
        val = None
        if self.lex.cur_token() in {Token.ADD}:
            if self.lex.cur_token() != Token.ADD:
                raise SyntaxError("Expected: ADD; on pos: " + str(self.lex.get_pos()))
            ADD = res.add_child(Token.ADD, self.lex.get_text())
            self.lex.next_token()
            mulDiv = res.add_child(self.mulDiv())
            next = left + mulDiv.val
            addSubP = res.add_child(self.addSubP(next))
            val = addSubP.val
        elif self.lex.cur_token() in {Token.SUB}:
            if self.lex.cur_token() != Token.SUB:
                raise SyntaxError("Expected: SUB; on pos: " + str(self.lex.get_pos()))
            SUB = res.add_child(Token.SUB, self.lex.get_text())
            self.lex.next_token()
            mulDiv = res.add_child(self.mulDiv())
            next = left - mulDiv.val
            addSubP = res.add_child(self.addSubP(next))
            val = addSubP.val
        else:
            val = left
        res.val = val
        return res

    class MulDiv(Universal):
        def __init__(self):
            super().__init__()
            self.val = None

    def mulDiv(self) -> MulDiv:
        res = self.MulDiv()
        val = None
        if self.lex.cur_token() in {Token.SUB, Token.NUM, Token.LPAREN}:
            unary = res.add_child(self.unary())
            mulDivP = res.add_child(self.mulDivP(unary.val))
            val = mulDivP.val
        else:
            raise SyntaxError("Expected: {'SUB', 'NUM', 'LPAREN'}; on pos: " + str(self.lex.get_pos()))
        res.val = val
        return res

    class MulDivP(Universal):
        def __init__(self):
            super().__init__()
            self.val = None

    def mulDivP(self, left) -> MulDivP:
        res = self.MulDivP()
        val = None
        if self.lex.cur_token() in {Token.MUL}:
            if self.lex.cur_token() != Token.MUL:
                raise SyntaxError("Expected: MUL; on pos: " + str(self.lex.get_pos()))
            MUL = res.add_child(Token.MUL, self.lex.get_text())
            self.lex.next_token()
            unary = res.add_child(self.unary())
            next = left * unary.val
            mulDivP = res.add_child(self.mulDivP(next))
            val = mulDivP.val
        elif self.lex.cur_token() in {Token.DIV}:
            if self.lex.cur_token() != Token.DIV:
                raise SyntaxError("Expected: DIV; on pos: " + str(self.lex.get_pos()))
            DIV = res.add_child(Token.DIV, self.lex.get_text())
            self.lex.next_token()
            unary = res.add_child(self.unary())
            next = left / unary.val
            mulDivP = res.add_child(self.mulDivP(next))
            val = mulDivP.val
        else:
            val = left
        res.val = val
        return res

    class Unary(Universal):
        def __init__(self):
            super().__init__()
            self.val = None

    def unary(self) -> Unary:
        res = self.Unary()
        val = None
        if self.lex.cur_token() in {Token.SUB}:
            if self.lex.cur_token() != Token.SUB:
                raise SyntaxError("Expected: SUB; on pos: " + str(self.lex.get_pos()))
            SUB = res.add_child(Token.SUB, self.lex.get_text())
            self.lex.next_token()
            unary = res.add_child(self.unary())
            val = -unary.val
        elif self.lex.cur_token() in {Token.LPAREN}:
            if self.lex.cur_token() != Token.LPAREN:
                raise SyntaxError("Expected: LPAREN; on pos: " + str(self.lex.get_pos()))
            LPAREN = res.add_child(Token.LPAREN, self.lex.get_text())
            self.lex.next_token()
            addSub = res.add_child(self.addSub())
            if self.lex.cur_token() != Token.RPAREN:
                raise SyntaxError("Expected: RPAREN; on pos: " + str(self.lex.get_pos()))
            RPAREN = res.add_child(Token.RPAREN, self.lex.get_text())
            self.lex.next_token()
            factorial = res.add_child(self.factorial(addSub.val))
            val = factorial.val
        elif self.lex.cur_token() in {Token.NUM}:
            if self.lex.cur_token() != Token.NUM:
                raise SyntaxError("Expected: NUM; on pos: " + str(self.lex.get_pos()))
            NUM = res.add_child(Token.NUM, self.lex.get_text())
            self.lex.next_token()
            factorial = res.add_child(self.factorial(int(NUM.text)))
            val = factorial.val
        else:
            raise SyntaxError("Expected: {'SUB', 'NUM', 'LPAREN'}; on pos: " + str(self.lex.get_pos()))
        res.val = val
        return res

    class Factorial(Universal):
        def __init__(self):
            super().__init__()
            self.val = None

    def factorial(self, num) -> Factorial:
        res = self.Factorial()
        val = None
        if self.lex.cur_token() in {Token.FACTOR}:
            if self.lex.cur_token() != Token.FACTOR:
                raise SyntaxError("Expected: FACTOR; on pos: " + str(self.lex.get_pos()))
            FACTOR = res.add_child(Token.FACTOR, self.lex.get_text())
            self.lex.next_token()
            from math import factorial as f
            val = f(num)
        else:
            val = num
        res.val = val
        return res
