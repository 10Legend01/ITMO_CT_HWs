package com.legend.android_6dz

import android.os.Bundle
import android.view.*
import androidx.fragment.app.Fragment
import androidx.navigation.fragment.findNavController
import androidx.navigation.fragment.navArgs
import com.legend.android_6dz.databinding.FragmentFirstBinding
import androidx.appcompat.app.AppCompatActivity

class FragmentUser : Fragment() {

    private val args: FragmentUserArgs by navArgs()

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        val binding = FragmentFirstBinding.inflate(layoutInflater)
        val tic = args.tic
        val text = args.text

        setHasOptionsMenu(true)

        val textView = binding.textView
        textView.text = text

        val button = binding.button
        button.setOnClickListener {
            findNavController().navigate(FragmentUserDirections.newFragmentUser().setTic(tic + 1).setText("$text->$tic"))
        }
        return binding.root
    }

    override fun onCreateOptionsMenu(menu: Menu, inflater: MenuInflater) {
        super.onCreateOptionsMenu(menu, inflater)
        (requireActivity() as AppCompatActivity).supportActionBar?.setDisplayHomeAsUpEnabled(true)
    }

}