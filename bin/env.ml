
type access = int
type ty = Types.ty

type enventry = VarEntry of {ty: ty; immutable: bool}
              | FunEntry of {formals: ty list; result: ty}

let base_tenv : ty Symbol.table = Symbol.empty (* predefined types *)
let base_tenv = Symbol.enter base_tenv (Symbol.symbol "int") Types.INT
let base_tenv = Symbol.enter base_tenv (Symbol.symbol "string") Types.STRING

let base_venv : enventry Symbol.table = Symbol.empty (* predefined functions *)
