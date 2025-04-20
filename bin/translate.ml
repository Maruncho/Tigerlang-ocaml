
type data_formal = Frame.data_formal

type level = Function of {this: Frame.frame; parent: level}
           | Outermost of {vframe: Frame.frame; stdlib: Temp.label list}
type access = level * Frame.access

let outermost = Outermost {vframe=Frame.newFrame (Symbol.symbol "<main>") false []; stdlib=[]}

let dummy_access = (outermost, Frame.dummy_access)

type newLevelArgs = {parent: level; name: Temp.label; needs_static_link: bool; formals: Frame.data_formal list}

let newLevel ({parent: level; name: Temp.label; needs_static_link: bool; formals: data_formal list}:newLevelArgs) =
    Function {this=(Frame.newFrame name needs_static_link formals); parent=parent}

let formals level = match level with
    | Outermost _ -> []
    | Function {this;_} ->
        List.map (fun fml -> (level, fml)) (Frame.formals this)

let allocLocal level data = match level with
    | Outermost {vframe;_} -> (level, Frame.allocLocal vframe data)
    | Function {this;_} -> (level, Frame.allocLocal this data)

let requiresStaticLink = function
    | Outermost _ -> false
    | Function {this;_} -> Frame.requiresStaticLink this
