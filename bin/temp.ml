
type temp = int

let debug = true

let temps = ref 100
let lbl = ref 0

let makeString tmp = "t" ^ (string_of_int tmp)

let newTemp ?(name=None) () = let t = !temps in let _ = temps := t + 1 in
                              let () = if debug && name <> None then print_string ((makeString t) ^ " : " ^ (Option.get name) ^ "\n") in
                              t


type label = Symbol.symbol

let newLabel () = Symbol.symbol ("L" ^ (string_of_int (let l = !lbl in let _ = lbl := l +1 in l)))

let namedLabel = Symbol.symbol
