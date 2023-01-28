import unittest
from parameterized import parameterized
from ..SyntaxAnalyzer import *


class TestLexicalAnalyzer(unittest.TestCase):

    @parameterized.expand([
        ["a"],  # проверка одной переменной
        ["(a)"],  # проверка скобок
        ["a not in b"],  # проверка not in
        ["a and b and c and d"],  # проверка ассоциативности основных бинарных операций
        ["a or b or c or d"],
        ["a xor b xor c xor d"],
        ["a and b or c xor d"],  # проверка всех основных бинарных операций вместе
        ["(a in b) or (c not in b)"],  # Проверка из примера
    ])
    # Проверка строк на вылет с правильным синтаксисом
    def test_ok(self, s):
        print(f"Test string:\n'{s}'")
        SyntaxAnalyzer(s=s).get_tree()

    @parameterized.expand([
        ["((a in b) and or not in a)"],  # проверка бинарных операций вместе
        ["(a))"],  # проверка скобочных последовательностей
        ["((a)"],
        ["a not not in a"],  # Проверка not not in
        ["a in b in c"],  # Перебор вариантов ассоциативности in и not in
        ["a not in b not in c"],
        ["a not in b in c"],
        ["a in b not in c"],
        [""],  # Тест на пустую строку
        ["()"],  # Тест на пустые скобки
    ])
    # Проверка строк с синтаксической ошибкой
    def test_error(self, s):
        print(f"Test string on error:\n'{s}'")
        snt = SyntaxAnalyzer(s=s)
        with self.assertRaises(SyntaxError) as _:
            snt.get_tree()


if __name__ == '__main__':
    unittest.main()
