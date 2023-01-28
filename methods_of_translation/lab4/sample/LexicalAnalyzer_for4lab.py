# здесь нужно указать откуда брать
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
        token_specification = [
            ('NUMBER', r'\d+(\.\d*)?'),  # Integer or decimal number
            ('ASSIGN', r':='),  # Assignment operator
            ('END', r';'),  # Statement terminator
            ('ID', r'[A-Za-z]+'),  # Identifiers
            ('OP', r'[+\-*/]'),  # Arithmetic operators
            ('NEWLINE', r'\n'),  # Line endings
            ('SKIP', r'[ \t]+'),  # Skip over spaces and tabs
            ('_MISMATCH_', r'.'),  # Any other character
        ]
        skip_tokens = { "SKIP" }
        tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)
        for mo in re.finditer(tok_regex, s):
            if mo.lastgroup == '_MISMATCH_':
                raise ParseException(f"Can't parse on pos: '{mo.pos}'")
            elif mo.lastgroup in skip_tokens:
                continue
            yield Token.__dict__[mo.lastgroup], mo.group()

    def next_token(self):
        self._save = next(self._generator)
        return self.cur_token()

    def cur_token(self):
        return self._save[0]

    def get_text(self):
        return self._save[1]
