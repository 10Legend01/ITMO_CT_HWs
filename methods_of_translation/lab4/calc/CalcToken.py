from enum import Enum


class Token(Enum):
    ADD = 0
    SUB = 1
    MUL = 2
    DIV = 3
    LPAREN = 4
    RPAREN = 5
    NUM = 6
    SKIP = 7
    end = 8


token_specification = [
    ('ADD', '\+'),
    ('SUB', '-'),
    ('MUL', '\*'),
    ('DIV', '/'),
    ('LPAREN', '\('),
    ('RPAREN', '\)'),
    ('NUM', '[0-9]+'),
    ('SKIP', '[\s]'),
    ('_MISMATCH_', '.'),
]

skip_tokens = {'SKIP'}
