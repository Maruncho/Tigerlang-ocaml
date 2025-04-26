
type level
type access

type exp

val outermost : level
val dummy_access : access
val dummy_exp : exp

val getResult : unit -> Frame.frag list

type newLevelArgs = {parent: level; name: Temp.label; needs_static_link: bool; formals: bool list}
val newLevel : newLevelArgs -> level

val formals : level -> access list
val allocLocal : level -> bool -> access

val requiresStaticLink : level -> bool

val unNx : exp -> Tree.stm

val intExp : int -> exp
val nilExp : unit -> exp
val unitExp : unit -> exp
val stringExp : string -> exp
val recordExp : exp list -> exp
val arrayExp : exp -> exp -> bool -> exp
val seqExp : exp list -> exp -> exp

val simpleVar : access -> level -> exp
val subscriptVar : exp -> exp -> exp
val fieldVar : exp -> int -> exp

val operArith : Absyn.oper -> Tree.binop
val operComp : Absyn.oper -> Tree.relop
val arith : exp -> exp -> Tree.binop -> exp
val comp : exp -> exp -> Tree.relop -> exp
val stringComp : exp -> exp -> Tree.relop -> exp

val assignExp : exp -> exp -> exp
val callExp : exp list -> level -> Temp.label -> exp

val ifExp : exp -> exp -> exp option -> exp
val letExp : exp list -> exp -> exp
val whileExp : exp -> exp -> Temp.label -> exp
val forExp : exp -> exp -> exp -> access -> Temp.label -> exp
val breakExp : Temp.label -> exp

val varDec : access -> exp -> exp
val funDec : exp -> level -> exp
