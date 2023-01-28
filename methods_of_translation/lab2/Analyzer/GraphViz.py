import graphviz
from .SyntaxAnalyzer import *


class GraphViz:
    dot = graphviz.Digraph(filename="GraphViz.gv")
    _num = None

    def draw_tree(self, tree: Tree):
        self.dot.clear()
        self._num = 0

        def _draw_tree(_num, _tree):
            if sum(map(lambda x: x in _tree.name, ('(', ')', 'Var', 'xor', 'or', 'and', 'not', 'in'))):
                self.dot.node(f"{_tree.name}_{_num}", style='filled')
            for child in _tree.children:
                self._num += 1
                self.dot.edge(f"{_tree.name}_{_num}", f"{child.name}_{self._num}")
                _draw_tree(self._num, child)

        _draw_tree(self._num, tree)
        self.dot.render()

    def draw_on_string(self, s: str):
        self.draw_tree(SyntaxAnalyzer(s=s).get_tree())


if __name__ == '__main__':
    s = "(a in b) or (c not in b)"
    g = GraphViz()
    g.draw_on_string(s)


# if __name__ == '__main__':
    # dot = graphviz.Digraph(directory="pic", filename=f"{datetime.datetime.now().strftime('%Y_%m_%d_%H_%M_%S')}.gv")
    # dot.node("A")
    # dot.node("P")
    # dot.node("V")
    # dot.edge("A", "P")
    # dot.edge("A0", "V")
    # dot.render()
