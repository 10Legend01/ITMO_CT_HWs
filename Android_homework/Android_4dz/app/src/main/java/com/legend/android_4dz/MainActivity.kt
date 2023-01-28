package com.legend.android_4dz

import android.content.ComponentName
import android.content.Intent
import android.content.ServiceConnection
import android.os.*
import androidx.appcompat.app.AppCompatActivity
import androidx.recyclerview.widget.LinearLayoutManager
import com.legend.android_4dz.databinding.ActivityMainBinding

var myService: MyService? = null

class MainActivity : AppCompatActivity() {

    lateinit var binding: ActivityMainBinding

    fun overrideView() {
        binding.myRecyclerView.adapter = UserAdapter(myService!!.bitmapList)
    }

    private val boundServiceConnection: ServiceConnection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName, service: IBinder) {
            val binderBridge: MyService.MyBinder = service as MyService.MyBinder
            myService = binderBridge.getMySirvice()
            myService!!.mainActivity = this@MainActivity
            overrideView()
        }

        override fun onServiceDisconnected(name: ComponentName) {
            myService!!.mainActivity = null
            myService = null
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        binding = ActivityMainBinding.inflate(layoutInflater)
        setContentView(binding.root)

        binding.myRecyclerView.layoutManager = LinearLayoutManager(this@MainActivity)
    }

    override fun onStart() {
        super.onStart()
        val intent = Intent(this, MyService::class.java)
        startService(intent)
        bindService(intent, boundServiceConnection, BIND_AUTO_CREATE)
    }

    override fun onStop() {
        super.onStop()
        if (myService != null && myService!!.bitmapList.isNotEmpty()) {
            unbindService(boundServiceConnection)
        }
    }

}