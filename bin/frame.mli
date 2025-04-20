
type frame
type access

type data_formal = {escape: bool; elementary: bool}

val dummy_access : access

val newFrame : Temp.label -> bool -> data_formal list -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val requiresStaticLink : frame -> bool
val allocLocal : frame -> data_formal -> access


