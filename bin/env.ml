
type access = Translate.access
type ty = Types.ty

type enventry = VarEntry of {access: Translate.access; ty: ty; immutable: bool}
              | FunEntry of {level: Translate.level; label: Temp.label; formals: ty list; result: ty}

let base_tenv : ty Symbol.table = Symbol.empty (* predefined types *)
let base_tenv = Symbol.enter base_tenv (Symbol.symbol "int") Types.INT
let base_tenv = Symbol.enter base_tenv (Symbol.symbol "string") Types.STRING

let base_venv : enventry Symbol.table = Symbol.empty (* predefined functions *)

(* Used mainly for array size sharing. Maybe ugly, but requires less refactoring.*)
let var_sizes : Translate.exp Symbol.table ref = ref Symbol.empty
