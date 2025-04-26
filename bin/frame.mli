
type frame
type access

type frag_proc = {body: Tree.stm; frame: frame}
type frag = PROC of frag_proc
          | STRING of Temp.label * string

val dummy_access : access

val sl_offset : int

val newFrame : Temp.label -> bool -> bool list -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val requiresStaticLink : frame -> bool
val allocLocal : frame -> bool -> access

val fp : Temp.temp
val rv : Temp.temp

val wordSize: int
val exp : access -> Tree.exp -> Tree.exp

val externalCall : string -> Tree.exp list -> Tree.exp

val procEntryExit1 : frame -> Tree.stm -> Tree.stm

