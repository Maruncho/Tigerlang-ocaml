
  type symbol = string * int

  exception Symbol
  let nexsym = ref 0
  let sizeHint = 128
  let hashTable : (string, int) Hashtbl.t = Hashtbl.create sizeHint

  let symbol name = match Hashtbl.find_opt hashTable name with
    | Some i -> name, i
    | None -> begin
		let i = !nexsym in nexsym := i + 1;
		Hashtbl.add hashTable name i;
		(name, i)
	      end
  
  let name ((s, _) : symbol) = s

  module IntMap = Map.Make(Int);;
  module Key = struct type key = symbol let getInt (_, n) = n end

  type key = Key.key
  type 'a table = 'a IntMap.t
  let empty = IntMap.empty
  let enter t k a = IntMap.add (Key.getInt k) a t
  let look t k = IntMap.find_opt (Key.getInt k) t

  let to_list = IntMap.to_list
