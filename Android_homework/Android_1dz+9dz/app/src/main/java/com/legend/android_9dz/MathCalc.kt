package com.legend.android_9dz

import java.lang.Exception

class MathCalc {
    var second = ""
    var first = ""
    var symb: Char = 0.toChar()

    fun getText(): String {
        return first + symb + second
    }

    fun count() {
        if (second.isNotEmpty() && first.isNotEmpty()) {
            try {
                second = when (symb) {
                    '+' -> {
                        (first.toDouble() + second.toDouble()).toString()
                    }
                    '-' -> {
                        (first.toDouble() - second.toDouble()).toString()
                    }
                    '÷' -> {
                        (first.toDouble() / second.toDouble()).toString()
                    }
                    '×' -> {
                        (first.toDouble() * second.toDouble()).toString()
                    }
                    else -> {
                        throw Exception()
                    }
                }
                first = ""
                symb = 0.toChar()
            }
            catch (e: Exception) {
                clear()
            }
        }
    }

    fun addNumber(c: Char) {
        second += c
    }

    fun addComma() {
        if (!second.contains('.')) {
            second += '.'
        }
    }

    fun removeLast() {
        if (second.isEmpty()) {
            symb = 0.toChar()
            second = first
            first = ""
        }
        else {
            second = second.substring(0, second.length - 1)
        }
    }

    fun clear() {
        second = ""
        first = ""
        symb = 0.toChar()
    }

    fun addSymb(c: Char) {
        if (second.isEmpty()) {
            symb = c
            if (first.isEmpty()) {
                first = "0"
            }
        }
        else {
            if (symb != 0.toChar()) {
                count()
            }
            first = second
            second = ""
            symb = c
        }
    }
}