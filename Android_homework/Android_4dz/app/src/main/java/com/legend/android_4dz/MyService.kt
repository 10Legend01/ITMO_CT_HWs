package com.legend.android_4dz

import android.content.Intent
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.os.Binder
import android.os.IBinder
import android.widget.Toast
import androidx.lifecycle.LifecycleService
import androidx.lifecycle.coroutineScope
import kotlinx.coroutines.*
import org.json.JSONArray
import java.net.HttpURLConnection
import java.net.URL

class MyService : LifecycleService() {

    var bitmapList = ArrayList<Pair<Bitmap, String>>()
    var bitmapFullList: MutableMap<String, Bitmap> = mutableMapOf()
    var full = ""
    var mainActivity: MainActivity? = null

    fun downloadBigImage() {
        if (!bitmapFullList.containsKey(full)) {
            bitmapFullList[full] = loadImage(full)
        }
    }

    fun loadImage(url: String): Bitmap = BitmapFactory.decodeStream((URL(url).openConnection() as HttpURLConnection).inputStream)

    private suspend fun getDataToDisplay() {
        try {
            if (bitmapList.isEmpty()) {
                val json = withContext(Dispatchers.IO) {
                    JSONArray(
                        (URL("https://api.unsplash.com/photos/random/?count=8;client_id=SGsbA0sZDqRsEfE60XaPYdysn7SQkB4iItPpekVtk4E")
                            .openConnection() as HttpURLConnection).inputStream.bufferedReader()
                            .readText()
                    )
                }
                for (i in 0 until json.length()) {
                    withContext(Dispatchers.IO) {
                        bitmapList.add(Pair(
                                loadImage(json.getJSONObject(i).getJSONObject("urls").getString("thumb")),
                                json.getJSONObject(i).getJSONObject("urls").getString("full")
                        ))
                    }
                }
            }
        }
        catch (e: Exception) {
            Toast.makeText(
                this,
                "Произошла ошибка при создании списка картинок.",
                Toast.LENGTH_LONG
            ).show()
        }

        mainActivity?.overrideView()
    }

    override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
        super.onStartCommand(intent, flags, startId)
        lifecycle.coroutineScope.launch {
            getDataToDisplay()
        }
        return START_NOT_STICKY
    }

    override fun onBind(intent: Intent): IBinder {
        super.onBind(intent)
        return MyBinder()
    }

    inner class MyBinder : Binder() {
        fun getMySirvice() = this@MyService
    }
}