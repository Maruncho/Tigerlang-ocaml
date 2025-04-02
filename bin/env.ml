
type access = int
type ty = Types.ty

type enventry = VarEntry of {ty: ty}
              | FunEntry of {formals: ty list; result: ty}

let base_tenv : ty Symbol.table = Symbol.empty (* predefined types *)
let base_venv : enventry Symbol.table = Symbol.empty (* predefined functions *)
