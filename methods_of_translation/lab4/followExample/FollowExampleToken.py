from enum import Enum


class Token(Enum):
    TOKEN = 0
    TOKENtwo = 1
    TOKENthree = 2
    end = 3


token_specification = [
    ('TOKEN', 'token'),
    ('TOKENtwo', 'token'),
    ('TOKENthree', 'token'),
    ('_MISMATCH_', '.'),
]

skip_tokens = set()
