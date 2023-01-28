if __name__ is not None and "." in __name__:
    from .FollowExampleToken import *
else:
    from FollowExampleToken import *


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
