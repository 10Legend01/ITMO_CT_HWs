from antlr4 import *
from KeyPrinter import *

def main(argv):
    input_stream = FileStream(argv)
    lexer = Var4Lexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = Var4Parser(stream)
    tree = parser.start()

    printer = KeyPrinter()
    walker = ParseTreeWalker()
    walker.walk(printer, tree)


if __name__ == '__main__':
    file = "C:/Users/Legend/PycharmProjects/Translation_lab3/test_input.txt"
    main(file)
