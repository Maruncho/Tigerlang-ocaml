
type temp
val newTemp : ?name:(string option) -> unit -> temp
(*module type Table = Map.S with type key = temp*)
val makeString : temp -> string

type label = Symbol.symbol
val newLabel : unit -> label
val namedLabel : string -> label

