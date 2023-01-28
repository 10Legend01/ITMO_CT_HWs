from enum import Enum


class Token(Enum):
    VAR = 1
    AND = 2
    OR = 3
    XOR = 4
    NOT = 5
    IN = 6
    # NOTIN = 7
    LPAREN = 8
    RPAREN = 9
    END = 10
