package com.legend.android_9dz

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.TextView
import android.content.ClipData
import android.content.ClipboardManager
import android.view.View

class MainActivity : AppCompatActivity() {

    private lateinit var tableText: TextView

    private var calc = MathCalc()

    private fun reloadText() {
        tableText.text = calc.getText()
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        tableText = findViewById(R.id.table_text)
        reloadText()
    }

    override fun onSaveInstanceState(outState: Bundle) {
        outState.putString("SECOND", calc.second)
        outState.putString("FIRST", calc.first)
        outState.putChar("SYMB", calc.symb)
        super.onSaveInstanceState(outState)
    }

    override fun onRestoreInstanceState(savedInstanceState: Bundle) {
        super.onRestoreInstanceState(savedInstanceState)
        calc.symb = savedInstanceState.getChar("SYMB", calc.symb)
        calc.first = savedInstanceState.getString("FIRST", calc.first)
        calc.second = savedInstanceState.getString("SECOND", calc.second)
        reloadText()
    }

    fun calcButton(view: View?) {
        when (view?.id) {
            R.id.but_copy -> {
                val clipboard: ClipboardManager = getSystemService(CLIPBOARD_SERVICE) as ClipboardManager
                val clip = ClipData.newPlainText("", tableText.text.toString())
                clipboard.setPrimaryClip(clip)
            }
            R.id.but_multiply -> { calc.addSymb('×') }
            R.id.but_divide -> { calc.addSymb('÷') }
            R.id.but_plus -> { calc.addSymb('+') }
            R.id.but_subtract -> { calc.addSymb('-') }
            R.id.but_equals -> { calc.count() }
            R.id.but_delete -> { calc.removeLast() }
            R.id.but_reset -> { calc.clear() }
            R.id.but_comma -> { calc.addComma() }
            R.id.but_0 -> { calc.addNumber('0') }
            R.id.but_1 -> { calc.addNumber('1') }
            R.id.but_2 -> { calc.addNumber('2') }
            R.id.but_3 -> { calc.addNumber('3') }
            R.id.but_4 -> { calc.addNumber('4') }
            R.id.but_5 -> { calc.addNumber('5') }
            R.id.but_6 -> { calc.addNumber('6') }
            R.id.but_7 -> { calc.addNumber('7') }
            R.id.but_8 -> { calc.addNumber('8') }
            R.id.but_9 -> { calc.addNumber('9') }
        }
        reloadText()
    }
}

