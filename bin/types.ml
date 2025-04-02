
type unique = unit ref

type ty = INT
        | STRING
        | RECORD of (Symbol.symbol * ty) list * string * unique
        | ARRAY of ty * string * unique
        | NIL
        | UNIT
        | NAME of Symbol.symbol * ty option ref
        | UNDEFINED
