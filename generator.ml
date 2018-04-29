(* Goal is to find a generator

We're using .txt files of the input *)

module Generator : 
	sig 
		type sentence
		type punctiation
		val read : parser -> (sentence * punctuation list)
		val present : (sentence * punctuation) list -> string list
	end