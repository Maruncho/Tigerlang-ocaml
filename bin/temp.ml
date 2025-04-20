
type temp = int

let temps = ref 100
let lbl = ref 0

let newTemp () = let t = !temps in let _ = temps := t + 1 in t

let makeString tmp = "t" ^ (string_of_int tmp)

type label = Symbol.symbol

let newLabel () = Symbol.symbol ("L" ^ (string_of_int (let l = !lbl in let _ = lbl := l +1 in l)))

let namedLabel = Symbol.symbol
