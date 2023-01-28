from lab2.Lab2Syntax import SyntaxAnalyzer
from GraphViz import *

if __name__ == '__main__':

    s = "(a in b) or (c not in b)"

    syn = SyntaxAnalyzer(s=s)
    tree = syn.get_tree()

    g = GraphViz()
    g.draw_tree(tree)

