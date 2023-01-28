package com.legend.android_4dz

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.Toast
import androidx.lifecycle.coroutineScope
import com.legend.android_4dz.databinding.BigItemBinding
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import java.lang.Exception

import android.graphics.Bitmap

class FullscreenActivity : AppCompatActivity() {

    private lateinit var binding: BigItemBinding

    private fun overrideView() {
        try {
            val image = myService!!.bitmapFullList[myService!!.full]

            val w = image!!.width
            val h = image.height

            val width = this@FullscreenActivity.window.decorView.width
            val height = (h * width) / w

            binding.imageBigView.setImageBitmap(
                Bitmap.createScaledBitmap(
                    image,
                    width,
                    height,
                    false
                )
            )
        } catch (e: Exception) {
            binding.imageBigView.setImageBitmap(myService!!.bitmapFullList[myService!!.full])
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        binding = BigItemBinding.inflate(layoutInflater)
        setContentView(binding.root)

        lifecycle.coroutineScope.launch {
            try {
                withContext(Dispatchers.IO) {
                    myService!!.downloadBigImage()
                }

                overrideView()

            } catch (e: Exception) {
                Toast.makeText(
                    this@FullscreenActivity,
                    "Что-то пошло не так...",
                    Toast.LENGTH_LONG
                ).show()
            }
        }

        binding.root.setOnClickListener { this.finish() }
    }
}