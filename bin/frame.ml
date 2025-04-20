
type data_formal = {escape: bool; elementary: bool}

type access = InFrame of int
            | InFramePtr of int
            | InReg of Temp.temp
            | InRegPtr of Temp.temp

(* the formals offset accounts for the ret address AND the locals offset accounts for the movq %rsp, %rbp instruction*)
type frame = {name: Temp.label; formals: access list; locals: (access list) ref; static_link: bool; last_locals_offset: int ref}

let dummy_access = InFrame 42069

let newFrame name static_link formals_data: frame =
    (* all data types are either 8 bytes or a 64bit pointer (8 bytes still) *)
    let last_offset = ref 0 in

    let (_, formals) = List.fold_left_map (fun cnt (data:data_formal) -> (
        let formal =
            if cnt < 6 then begin
                let temp = Temp.newTemp() in
                if (data.escape || (not data.elementary)) then InRegPtr temp else InReg temp end
            else begin
                let () = last_offset := !last_offset + 8 in
                if data.elementary then InFrame !last_offset else InFramePtr !last_offset end
        in ((cnt+1), formal)
    )) 0 formals_data in
        {name=name; formals=formals; locals= ref []; static_link=static_link; last_locals_offset= ref 0}

let name frame = frame.name
let formals frame = frame.formals
let requiresStaticLink frame = frame.static_link
let allocLocal frame (data:data_formal) =
    let access = match data with
        | {escape=false; elementary=false} -> InRegPtr (Temp.newTemp())
        | {escape=false; elementary=true} -> InReg (Temp.newTemp())
        | {escape=true; elementary=false} ->
            let () = frame.last_locals_offset := !(frame.last_locals_offset) + 8 in
            InFramePtr !(frame.last_locals_offset)
        | {escape=true; elementary=true} ->
            let () = frame.last_locals_offset := !(frame.last_locals_offset) + 8 in
            InFrame !(frame.last_locals_offset)
    in let () = frame.locals := (!(frame.locals) @ [access]) in access
