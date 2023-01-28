package com.legend.android_3dz

import android.Manifest
import android.annotation.SuppressLint
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.Toast
import androidx.recyclerview.widget.LinearLayoutManager
import com.legend.android_3dz.databinding.ActivityMainBinding
import android.content.Context
import android.content.pm.PackageManager
import android.provider.ContactsContract
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat

class MainActivity : AppCompatActivity() {

    private val readContactsId = 1

    lateinit var binding: ActivityMainBinding

    data class Contact(val name: String, val phoneNumber: String)

    var usersList = (0..10).map {
        Contact("", "")
    }

    @SuppressLint("Range", "Recycle")
    fun Context.fetchAllContacts(): List<Contact> {
        val cursor = contentResolver.query(
            ContactsContract.CommonDataKinds.Phone.CONTENT_URI, null, null, null, null
        ) ?: return emptyList()
        val builder = ArrayList<Contact>()
        while (cursor.moveToNext()) {
            val name = cursor.getString(cursor.getColumnIndex(ContactsContract.CommonDataKinds.Phone.DISPLAY_NAME)) ?: "N/A"
            val phoneNumber = cursor.getString(cursor.getColumnIndex(ContactsContract.CommonDataKinds.Phone.NUMBER)) ?: "N/A"
            builder.add(Contact(name, phoneNumber))
        }
        return builder
    }

    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<String>,
        grantResults: IntArray)
    {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
        when (requestCode) {
            readContactsId -> {
                if (grantResults.isNotEmpty() && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    reloadList()
                }
                else {
                    Toast.makeText(
                        this@MainActivity,
                        "Для работы приложения требуется доступ к контактам.",
                        Toast.LENGTH_LONG
                    ).show()
                }
            }
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        binding = ActivityMainBinding.inflate(layoutInflater)
        setContentView(binding.root)

        binding.myRecyclerView.apply {
            layoutManager = LinearLayoutManager(this@MainActivity)
            adapter = UserAdapter(usersList)
        }

        if (ContextCompat.checkSelfPermission(
                this@MainActivity,
                Manifest.permission.READ_CONTACTS) != PackageManager.PERMISSION_GRANTED)
        {
            ActivityCompat.requestPermissions(
                this@MainActivity,
                arrayOf(Manifest.permission.READ_CONTACTS),
                readContactsId)
        }
        else {
            reloadList()
        }
    }

    fun reloadList() {
        usersList = fetchAllContacts()
        binding.myRecyclerView.adapter = UserAdapter(usersList)

        val count = usersList.size
        val lenContacts = resources.getQuantityString(R.plurals.numberOfContacts, count, count)

        Toast.makeText(
            this@MainActivity,
            lenContacts,
            Toast.LENGTH_SHORT
        ).show()
    }

}
