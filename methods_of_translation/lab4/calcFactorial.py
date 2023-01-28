from calcFactorial.CalcFactorialSyntax import SyntaxAnalyzer
from GraphViz import *

if __name__ == '__main__':

    s = "-(1 + 2)! / (3 * 4)!"

    syn = SyntaxAnalyzer(s=s)
    tree = syn.get_tree()

    g = GraphViz()
    g.draw_tree(tree)
