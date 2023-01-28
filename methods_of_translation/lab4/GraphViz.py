import graphviz


class GraphViz:

    _num = None

    def __init__(self, filename="GraphViz.gv", directory=None):
        self.dot = graphviz.Digraph(filename=filename, directory=directory)

    def draw_tree(self, tree):
        self.dot.clear()
        self._num = 0

        def _get_vars(_tree) -> str:
            cp = _tree.__dict__.copy()
            cp.pop("children")
            if cp:
                return '\n' + str(cp)
            return ""

        def _draw_tree(_num, _tree):
            if "children" not in _tree.__dict__:
                self.dot.node(f"{_tree.__class__.__name__}_{_num}", style='filled', label=_tree.text)
                return
            self.dot.node(f"{_tree.__class__.__name__}_{_num}", label=_tree.__class__.__name__ + _get_vars(_tree))
            for child in _tree.children:
                self._num += 1
                self.dot.edge(f"{_tree.__class__.__name__}_{_num}", f"{child.__class__.__name__}_{self._num}")
                _draw_tree(self._num, child)

        _draw_tree(self._num, tree)
        self.dot.render()



# if __name__ == '__main__':
#     s = "(a in b) or (c not in b)"
#
#     syn = SyntaxAnalyzer(s=s)
#     get = syn.get_tree()
#
#     g = GraphViz()
#     g.draw_tree(get)

