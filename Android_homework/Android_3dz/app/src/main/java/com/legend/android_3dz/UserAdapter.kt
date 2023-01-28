package com.legend.android_3dz

import android.content.Intent
import android.net.Uri
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView

class UserAdapter(private val users: List<MainActivity.Contact>) : RecyclerView.Adapter<UserAdapter.UserViewHolder>() {

    class UserViewHolder(root: View) : RecyclerView.ViewHolder(root) {
        val nameView = root.findViewById<TextView>(R.id.first_name)
        val phoneNumberView = root.findViewById<TextView>(R.id.last_name)

        fun bind(user: MainActivity.Contact) {
            nameView.text = user.name
            phoneNumberView.text = user.phoneNumber
        }
    }

    override fun getItemCount() = users.size

    override fun onBindViewHolder(
        holder: UserViewHolder,
        position: Int
    ) = holder.bind(users[position])

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int)
            : UserViewHolder {
        val holder = UserViewHolder(
            LayoutInflater
                .from(parent.context)
                .inflate(R.layout.list_item, parent, false)
        )
        holder.nameView.setOnClickListener {
            parent.context.startActivity(
                Intent(
                Intent.ACTION_SENDTO,
                Uri.parse("smsto:"+users[holder.adapterPosition].phoneNumber)
            ).putExtra("sms_body", "Hi!\n"))
        }
        holder.phoneNumberView.setOnClickListener {
            parent.context.startActivity(
                Intent(
                Intent.ACTION_DIAL,
                Uri.parse("tel:"+users[holder.adapterPosition].phoneNumber))
            )
        }
        return holder
    }
}