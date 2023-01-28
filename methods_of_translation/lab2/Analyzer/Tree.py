
class Tree:
    def __init__(self, name: str, children=None):
        self.children: list = list()
        self.name: str = str()
        if children is not None:
            isinstance(children, list)
            self.children: list = children
        self.name: str = name

    def add_child(self, child):
        self.children.append(child)
