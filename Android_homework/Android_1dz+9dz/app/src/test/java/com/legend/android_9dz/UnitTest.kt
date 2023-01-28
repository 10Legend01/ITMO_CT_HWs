package com.legend.android_9dz

import org.junit.Assert.*
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import org.mockito.Mock
import org.mockito.junit.MockitoJUnitRunner

@RunWith(MockitoJUnitRunner::class)
class MathCalcUnitTest {

    @Mock lateinit var calc: MathCalc

    private fun numberWithComma() {
        calc.addNumber('1')
        calc.addComma()
        calc.addNumber('1')
    }

    private fun plus2With2() {
        calc.addNumber('2')
        calc.addSymb('+')
        calc.addNumber('2')
    }

    @Before
    fun setUp() {
        calc = MathCalc()
    }

    @Test
    fun addOneNumeral() {
        calc.addNumber('1')
        assertEquals(1.0, calc.getText().toDouble(), 0.0)
    }

    @Test
    fun addMoreNumerals() {
        calc.addNumber('2')
        assertEquals(2.0, calc.getText().toDouble(), 0.0)
        calc.addNumber('3')
        assertEquals(23.0, calc.getText().toDouble(), 0.0)
        calc.addNumber('4')
        assertEquals(234.0, calc.getText().toDouble(), 0.0)
    }

    @Test
    fun removeNumeral() {
        calc.addNumber('1')
        calc.addNumber('1')
        assertEquals(11.0, calc.getText().toDouble(), 0.0)
        calc.removeLast()
        assertEquals(1.0, calc.getText().toDouble(), 0.0)
    }

    @Test
    fun addComma() {
        numberWithComma()
        assertEquals(1.1, calc.getText().toDouble(), 0.0)
    }

    @Test
    fun removeComma() {
        numberWithComma()
        calc.removeLast()
        calc.removeLast()
        assertEquals(1.0, calc.getText().toDouble(), 0.0)
    }

    @Test
    fun calculateSimpleExpression() {
        plus2With2()
        calc.count()
        assertEquals(4.0, calc.getText().toDouble(), 0.0)
    }

    @Test
    fun removeSymbol() {
        plus2With2()
        calc.removeLast()
        calc.removeLast()
        assertEquals(2.0, calc.getText().toDouble(), 0.0)
    }

    @Test
    fun oneDivideZero() {
        calc.addNumber('1')
        calc.addSymb('÷')
        calc.addNumber('0')
        calc.count()
        assertTrue(calc.getText().toDouble().isInfinite())
    }

    @Test
    fun zeroDivideZero() {
        calc.addNumber('0')
        calc.addSymb('÷')
        calc.addNumber('0')
        calc.count()
        assertTrue(calc.getText().toDouble().isNaN())
    }

    @Test
    fun reset() {
        calc.addNumber('1')
        calc.addNumber('2')
        assertEquals(12.0, calc.getText().toDouble(), 0.0)
        calc.clear()
        calc.addNumber('3')
        calc.addNumber('4')
        assertEquals(34.0, calc.getText().toDouble(), 0.0)
    }

    @Test
    fun checkDouble() {
        numberWithComma()
        calc.addSymb('+')
        calc.addNumber('2')
        calc.addComma()
        calc.addNumber('2')
        calc.count()
        assertFalse(calc.getText().toDouble() == 3.3)
        assertEquals(3.3, calc.getText().toDouble(), 0.00001)
    }

}
