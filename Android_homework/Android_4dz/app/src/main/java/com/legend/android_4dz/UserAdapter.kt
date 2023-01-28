package com.legend.android_4dz

import android.content.Intent
import android.graphics.Bitmap
import android.view.LayoutInflater
import android.view.ViewGroup
import androidx.recyclerview.widget.RecyclerView
import com.legend.android_4dz.databinding.ListItemBinding

class UserAdapter(private val images: List<Pair<Bitmap, String>>) : RecyclerView.Adapter<UserAdapter.UserViewHolder>() {

    class UserViewHolder(root: ListItemBinding) : RecyclerView.ViewHolder(root.root) {
        val imageView = root.imageView
        lateinit var url: String

        fun bind(pic: Pair<Bitmap, String>) {
            imageView.setImageBitmap(pic.first)
            url = pic.second
        }
    }

    override fun getItemCount() = images.size

    override fun onBindViewHolder(
        holder: UserViewHolder,
        position: Int
    ) = holder.bind(images[position])

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int) : UserViewHolder {

        val holder = UserViewHolder(
            ListItemBinding.inflate(
                LayoutInflater.from(parent.context),
                        parent, false
            )
        )

        holder.imageView.setOnClickListener {
            myService!!.full = holder.url
            parent.context.startActivity(
                Intent(parent.context, FullscreenActivity::class.java)
            )
        }
        return holder
    }
}
