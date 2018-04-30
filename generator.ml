(* Goal is to find a generator

We're using .txt files of the input *)

module type GENERATOR =
	sig 
		val empty: string
		val roll : Markovchain.roll
		val present : Markovchain.mchain -> string -> string list
	end

module Generator : GENERATOR = 
	struct
		let rec gen (m: mchain) (word: string) : string list  =
			match roll m word with
			|Some w -> w :: gen m w
			|None -> []
	end
