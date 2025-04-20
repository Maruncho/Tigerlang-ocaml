
type pos = int
and symbol = Symbol.symbol
and oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
and field =  {name: symbol; escape: bool ref; typ: symbol; pos: pos}

type var= SimpleVar of symbol * pos
        | FieldVar of var * symbol * pos
        | SubscriptVar of var * expr * pos

and expr =
        | VarExp of var
        | NilExp
        | IntExp of int
        | StringExp of string * pos
        | CallExp of {func: symbol; args: expr list; pos: pos}
        | OpExp of {left: expr; oper: oper; right: expr; pos: pos}
        | RecordExp of {fields: (symbol * expr * pos) list; typ: symbol; pos: pos}
        | SeqExp of (expr list * pos)
        | AssignExp of {var: var; expr: expr; pos: pos}
        | IfExp of {test: expr; then': expr; else': expr option; pos: pos}
        | WhileExp of {test: expr; body: expr; pos: pos}
        | ForExp of {var: symbol; escape: bool ref; lo: expr; hi:expr; body: expr; pos: pos}
        | BreakExp of pos
        | LetExp of {decs: dec list; body: expr; pos: pos}
        | ArrayExp of {typ: symbol; size: expr; init: expr; pos: pos}

and fundec = {name: symbol; params: field list; result: (symbol * pos) option;
              body: expr; static_link: bool ref; pos: pos}

and ty  = NameTy of symbol * pos
        | RecordTy of field list
        | ArrayTy of symbol * pos

and typeDecRecord = {name: symbol; ty: ty; pos: pos}

and dec = FunctionDec of fundec list
        | VarDec of {name: symbol; escape: bool ref; typ: (symbol * pos) option; init: expr; pos: pos}
        | TypeDec of typeDecRecord list
