if __name__ is not None and "." in __name__:
    from .FollowExampleToken import *
    from .FollowExampleLexical import *
else:
    from FollowExampleToken import *
    from FollowExampleLexical import *


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
        tree = self.start()
        if self.lex.cur_token() != Token.end:
            raise SyntaxError("End of statement expected, found %s on pos %s" 
                              % self.lex.cur_token(), self.lex.get_pos()
                              )
        return tree

    class Start(Universal):
        def __init__(self):
            super().__init__()
            
    def start(self) -> Start:
        res = self.Start()
        if self.lex.cur_token() in {Token.TOKEN, Token.TOKENthree, Token.TOKENtwo}:
            haveEPS = res.add_child(self.haveEPS())
            haveEPSTwo = res.add_child(self.haveEPSTwo())
            if self.lex.cur_token() != Token.TOKENthree:
                raise SyntaxError("Expected: TOKENthree; on pos: " + str(self.lex.get_pos()))
            TOKENthree = res.add_child(Token.TOKENthree, self.lex.get_text())
            self.lex.next_token()
        else:
            pass
        return res

    class HaveEPS(Universal):
        def __init__(self):
            super().__init__()
            
    def haveEPS(self) -> HaveEPS:
        res = self.HaveEPS()
        if self.lex.cur_token() in {Token.TOKEN}:
            if self.lex.cur_token() != Token.TOKEN:
                raise SyntaxError("Expected: TOKEN; on pos: " + str(self.lex.get_pos()))
            TOKEN = res.add_child(Token.TOKEN, self.lex.get_text())
            self.lex.next_token()
        else:
            pass
        return res

    class HaveEPSTwo(Universal):
        def __init__(self):
            super().__init__()
            
    def haveEPSTwo(self) -> HaveEPSTwo:
        res = self.HaveEPSTwo()
        if self.lex.cur_token() in {Token.TOKENtwo}:
            if self.lex.cur_token() != Token.TOKENtwo:
                raise SyntaxError("Expected: TOKENtwo; on pos: " + str(self.lex.get_pos()))
            TOKENtwo = res.add_child(Token.TOKENtwo, self.lex.get_text())
            self.lex.next_token()
        else:
            pass
        return res
