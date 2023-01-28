from enum import Enum


class Token(Enum):
    XOR = 0
    OR = 1
    AND = 2
    NOT = 3
    IN = 4
    VAR = 5
    LPAR = 6
    RPAR = 7
    SKIP = 8
    end = 9


token_specification = [
    ('XOR', 'xor'),
    ('OR', 'or'),
    ('AND', 'and'),
    ('NOT', 'not'),
    ('IN', 'in'),
    ('VAR', '[a-z]'),
    ('LPAR', '\('),
    ('RPAR', '\)'),
    ('SKIP', '[\s]'),
    ('_MISMATCH_', '.'),
]

skip_tokens = {'SKIP'}
