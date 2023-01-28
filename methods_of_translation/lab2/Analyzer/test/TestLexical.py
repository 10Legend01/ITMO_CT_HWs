import unittest
from parameterized import parameterized
from ..LexicalAnalyzer import *
from ..Token import *


class TestLexicalAnalyzer(unittest.TestCase):

    @parameterized.expand([
        [  # Проверка из примера
            "(a in b) or (c not in b)",
            [
                (Token.LPAREN, None),
                (Token.VAR, 'a'),
                (Token.IN, None),
                (Token.VAR, 'b'),
                (Token.RPAREN, None),
                (Token.OR, None),
                (Token.LPAREN, None),
                (Token.VAR, 'c'),
                (Token.NOT, None),
                (Token.IN, None),
                (Token.VAR, 'b'),
                (Token.RPAREN, None),
                (Token.END, None)
            ]
        ],
        [  # Проверка пустой строки
            "",
            [
                (Token.END, None)
            ]
        ],
        [  # Проверка маленькой буквы переменной
            "a",
            [
                (Token.VAR, 'a'),
                (Token.END, None)
            ]
        ],
        [  # Проверка большой буквы переменной
            "A",
            [
                (Token.VAR, 'A'),
                (Token.END, None)
            ]
        ],
        [   # Проверка на правильность лексики, но с неправильным синтаксисом
            "((a in b) and or not not in a)",
            [
                (Token.LPAREN, None),
                (Token.LPAREN, None),
                (Token.VAR, 'a'),
                (Token.IN, None),
                (Token.VAR, 'b'),
                (Token.RPAREN, None),
                (Token.AND, None),
                (Token.OR, None),
                (Token.NOT, None),
                (Token.NOT, None),
                (Token.IN, None),
                (Token.VAR, 'a'),
                (Token.RPAREN, None),
                (Token.END, None),
            ]
        ],
        [  # Перебор абсолютно всех возможных токенов
            "( ) xor or and in not in a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N "
            "O P Q R S T U V W X Y Z",
            [
                (Token.LPAREN, None),
                (Token.RPAREN, None),
                (Token.XOR, None),
                (Token.OR, None),
                (Token.AND, None),
                (Token.IN, None),
                (Token.NOT, None),
                (Token.IN, None),
                (Token.VAR, 'a'),
                (Token.VAR, 'b'),
                (Token.VAR, 'c'),
                (Token.VAR, 'd'),
                (Token.VAR, 'e'),
                (Token.VAR, 'f'),
                (Token.VAR, 'g'),
                (Token.VAR, 'h'),
                (Token.VAR, 'i'),
                (Token.VAR, 'j'),
                (Token.VAR, 'k'),
                (Token.VAR, 'l'),
                (Token.VAR, 'm'),
                (Token.VAR, 'n'),
                (Token.VAR, 'o'),
                (Token.VAR, 'p'),
                (Token.VAR, 'q'),
                (Token.VAR, 'r'),
                (Token.VAR, 's'),
                (Token.VAR, 't'),
                (Token.VAR, 'u'),
                (Token.VAR, 'v'),
                (Token.VAR, 'w'),
                (Token.VAR, 'x'),
                (Token.VAR, 'y'),
                (Token.VAR, 'z'),
                (Token.VAR, 'A'),
                (Token.VAR, 'B'),
                (Token.VAR, 'C'),
                (Token.VAR, 'D'),
                (Token.VAR, 'E'),
                (Token.VAR, 'F'),
                (Token.VAR, 'G'),
                (Token.VAR, 'H'),
                (Token.VAR, 'I'),
                (Token.VAR, 'J'),
                (Token.VAR, 'K'),
                (Token.VAR, 'L'),
                (Token.VAR, 'M'),
                (Token.VAR, 'N'),
                (Token.VAR, 'O'),
                (Token.VAR, 'P'),
                (Token.VAR, 'Q'),
                (Token.VAR, 'R'),
                (Token.VAR, 'S'),
                (Token.VAR, 'T'),
                (Token.VAR, 'U'),
                (Token.VAR, 'V'),
                (Token.VAR, 'W'),
                (Token.VAR, 'X'),
                (Token.VAR, 'Y'),
                (Token.VAR, 'Z'),
                (Token.END, None),
            ]
        ],
    ])
    # Тестирование правильной последовательности токенов
    def test_ok(self, s, ans):
        print(f"Test string:\n'{s}'")
        lex = LexicalAnalyzer(s)
        assert list(lex) == ans

    @parameterized.expand([
        ["1"],  # запрет цифр
        ["123"],  # проверка переменной с длиной больше 1 и с цифрами
        ["abc"],  # проверка переменной с длиной больше 1
        ["(a in b abc)"],  # проверка переменной с длиной больше 1 в небольшом выражении
    ])
    # Тестирование строк с лексической ошибкой
    def test_error(self, s):
        print(f"Test string on error:\n'{s}'")
        lex = LexicalAnalyzer(s)
        with self.assertRaises(ParseException) as _:
            list(lex)


def p():
    s = "( ) xor"
    lex = LexicalAnalyzer(s)
    print('[')
    for i in lex:
        if i[1] is None:
            print(f"({str(i[0])}, {i[1]}),")
        else:
            print(f"({str(i[0])}, '{i[1]}'),")
    print(']')


# p()
if __name__ == '__main__':
    unittest.main()
