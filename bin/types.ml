
type unique = unit ref

type ty = INT
        | STRING
        | RECORD of (Symbol.symbol * ty) list * string * unique
        | REC_RECORD of ty ref (* Pretty sure recursive definitions need mutability, but I may just suck at FP*)
        | ARRAY of ty * string * unique
        | NIL (*All records can be nil or their respective type. Nil is just that.*)
        | UNIT (*Doesn't have literal, but it is a type.*)
        | NAME of Symbol.symbol * ty option ref (*intermediate representation, in impl details. More complicated than it should be, but it ain't that bad; I didn't quite follow the book's idea, but kept it as is anyway*)
        | UNDEFINED (*A 'native' 'anti-' type. Doesn't have a literal, it's not a real type.*)
