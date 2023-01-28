package com.legend.android_5dz

import android.content.Context
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.os.Parcelable
import android.util.AttributeSet
import android.view.View
import android.os.Parcel


class MyView @JvmOverloads constructor(context: Context, attrs: AttributeSet, defStyleAttr: Int = 0, defStyleRes: Int = 0) :
    View(context, attrs, defStyleAttr, defStyleRes) {

    class MyBaseSavedState : BaseSavedState {
        var timer : Float

        constructor(superState: Parcelable?, timer: Float) : super(superState) {
            this.timer = timer
        }

        constructor(superState: Parcel) : super(superState) {
            this.timer = superState.readFloat()
        }

        override fun writeToParcel(out: Parcel?, flags: Int) {
            super.writeToParcel(out, flags)
            out?.writeFloat(timer)
        }

        companion object CREATOR : Parcelable.Creator<MyBaseSavedState?> {
            override fun createFromParcel(source: Parcel): MyBaseSavedState {
                return MyBaseSavedState(source)
            }

            override fun newArray(size: Int): Array<MyBaseSavedState?> {
                return arrayOfNulls(size)
            }
        }
    }

    private var myPaint : Paint? = null

    private var timer = 0f
    private val maxTimer = 360f

    init {
        myPaint = Paint()
        myPaint?.color = Color.RED
        myPaint?.flags = Paint.ANTI_ALIAS_FLAG
        myPaint?.strokeWidth = 23f
    }

    override fun onDraw(canvas: Canvas) {
        val h = this.height
        val w = this.width

        val startX = 0f + (w.toFloat() * timer / maxTimer)
        val startY = 0f + (h.toFloat() * timer / maxTimer)

        val endX = w.toFloat() - startX
        val endY = h.toFloat() - startY

        canvas.drawLine(startX, 0f, endX, h.toFloat(), myPaint!!)
        canvas.drawLine(w.toFloat(), startY, 0f, endY, myPaint!!)

        timer += 1f
        if (timer > maxTimer) {
            timer = 0f
        }

        this.invalidate()
    }

    override fun onSaveInstanceState(): Parcelable {
        return MyBaseSavedState(super.onSaveInstanceState(), timer)
    }

    override fun onRestoreInstanceState(state: Parcelable?) {
        val savedState = state as MyBaseSavedState
        super.onRestoreInstanceState(savedState.superState)
        timer = savedState.timer
    }

}
