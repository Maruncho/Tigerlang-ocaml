
type access = InFrame of int
            | InReg of Temp.temp

(* the formals offset accounts for the ret address AND the locals offset accounts for the movq %rsp, %rbp instruction*)
type frame = {name: Temp.label; formals: access list; locals: (access list) ref; static_link: bool; last_locals_offset: int ref}


type frag_proc = {body: Tree.stm; frame: frame}
type frag = PROC of frag_proc
          | STRING of Temp.label * string

let dummy_access = InFrame 42069

let fp = Temp.newTemp() ~name:(Some "FP")
let rv = Temp.newTemp() ~name:(Some "RV")

let wordSize = 8

let sl_offset = wordSize


let newFrame name static_link escapes : frame =
    (* all data types are either 8 bytes or a 64bit pointer (8 bytes still) *)
    let last_offset = ref (if static_link then 8 else 0) in

    let formals = List.map (fun (escape:bool) -> (
        if (not escape)
        then
            InReg (Temp.newTemp() ~name:(Some ((Symbol.name name) ^ " formal")))
        else
            let () = last_offset := !last_offset + wordSize in
            InFrame !last_offset
    )) escapes in
        {name=name; formals=formals; locals= ref []; static_link=static_link; last_locals_offset= ref 0}

let name frame = frame.name
let formals frame = frame.formals
let requiresStaticLink frame = frame.static_link
let allocLocal frame (escape:bool) =
    let access = match escape with
        | false -> InReg (Temp.newTemp() ~name:(Some ((Symbol.name frame.name) ^ " local")))
        | true ->
            let () = frame.last_locals_offset := !(frame.last_locals_offset) - wordSize in
            InFrame !(frame.last_locals_offset)
    in let () = frame.locals := (!(frame.locals) @ [access]) in access


let offsetFormula fp off = if off < 0 then (Tree.BINOP (Tree.MINUS, fp, Tree.CONST(- off)))
                                                  else (Tree.BINOP (Tree.PLUS, fp, Tree.CONST off))

let exp access stackFrameAddr = match access with
    | InFrame addr -> Tree.MEM (offsetFormula stackFrameAddr addr)
    | InReg temp -> Tree.TEMP temp

let externalCall name exps =
    let () = match name with
        | "malloc"
        | "initArray"
        | "initArrayCopy"
        | "stringCompare" -> ()
        | _ -> failwith (name ^ " is not an external function.")

    in Tree.CALL (Tree.NAME (Temp.namedLabel name), exps)


(* TODO: Actually implement later in the book*)
let procEntryExit1 frame body = let _ = frame in body
