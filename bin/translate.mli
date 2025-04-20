
type level
type access

type data_formal = Frame.data_formal

val outermost : level

val dummy_access : access

type newLevelArgs = {parent: level; name: Temp.label; needs_static_link: bool; formals: Frame.data_formal list}
val newLevel : newLevelArgs -> level

val formals : level -> access list
val allocLocal : level -> data_formal -> access

val requiresStaticLink : level -> bool
