from typing import List, Tuple


class Term:
    def __init__(self, name, expr, skip):
        self.name: str = name
        self.expr: str = expr
        self.skip: bool = skip


class NonTerm:
    TERM = 1
    NONTERM = 2
    CODE = 3

    def __init__(self, name, args, returns, rules):
        self.name: str = name
        self.args: List[str] = args
        self.returns: List[str] = returns
        self.rules: List[Tuple[int, str, ]] = rules
