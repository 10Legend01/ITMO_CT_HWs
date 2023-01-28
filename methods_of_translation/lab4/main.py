from antlr4 import *
from generator.MyVisitor import *
from generator.Analyzer import *


def main(*args):
    Analyzer(*args)


if __name__ == '__main__':
    # file = "grm/test.grm"

    file = "grm/calc.grm"
    main(file, "Calc", "calc")

    # file = "grm/lab2.grm"
    # main(file, "Lab2", "lab2")

    # file = "grm/calcFactorial.grm"
    # main(file, "CalcFactorial", "calcFactorial")

    # file = "grm/FollowExample.grm"
    # main(file, "FollowExample", "followExample")
