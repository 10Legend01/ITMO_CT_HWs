from .Token import *
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
    def _token_gen(s: str) -> (Token, str):
        r = r"(\(|\)|[^\s\(\)]+)"
        for w in re.findall(r, s):
            if w == "and":
                yield Token.AND, None
            elif w == "or":
                yield Token.OR, None
            elif w == "xor":
                yield Token.XOR, None
            elif w == "not":
                yield Token.NOT, None
            elif w == "in":
                yield Token.IN, None
            elif w == "(":
                yield Token.LPAREN, None
            elif w == ")":
                yield Token.RPAREN, None
            elif len(w) == 1 and ('a' <= w <= 'z' or 'A' <= w <= 'Z'):
                yield Token.VAR, w
            else:
                raise ParseException(f"Can't parse that: '{w}'")
        yield Token.END, None

    def __next__(self):
        # return self.next_token()
        return self.next_token(), self.get_message()

    def __iter__(self):
        return self

    def next_token(self):
        self._save = next(self._generator)
        return self.cur_token()

    def cur_token(self):
        return self._save[0]

    def get_message(self):
        return self._save[1]
