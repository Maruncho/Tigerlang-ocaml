
module A = Absyn


type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table

type expty = {exp: unit; ty: Types.ty}

let placeholder_unique = ref ()
let placeholder_rec = Types.RECORD ([], "", placeholder_unique)

let rec list3split = function
    | [] -> ([],[],[])
    | (x,y,z) :: t -> let (xt, yt, zt) = list3split t in
                      (x::xt, y::yt, z::zt)

let rec pop_in_list f = function
    | [] -> (None, [])
    | h :: t -> if (f h)
                then ((Some h), t)
                else let (found, rest) = pop_in_list f t in
                     (found, h :: rest)

let string_of_cycle names =
    let first = List.hd names in
    let rec iter = function
        | [] -> first
        | name :: t -> (name ^ " -> " ^ iter t)
    in iter names

let rec string_of_type ?(verbose=false) ?(show_recursive=false) typ = match typ with
    | Types.INT -> "int"
    | Types.STRING -> "string"
    | Types.NIL -> "nil"
    | Types.UNIT -> "unit"
    | Types.ARRAY (ty, name, _) -> name ^ "[" ^ (string_of_type ty) ^ "]"
    | Types.RECORD (fields, name, _) -> if verbose
                                        then name ^ "{" ^ (List.fold_left (fun acc (sym, ty) -> acc ^ " " ^ (Symbol.name sym) ^ ": " ^ (string_of_type ~verbose:false ty) ^ ";") "" fields) ^ " }"
                                        else "{}" ^ name
    | Types.REC_RECORD (ty) -> (match !ty with Types.RECORD (_,_,_) -> (if show_recursive then "{rec}" else "") ^ (string_of_type ~verbose:verbose !ty) | _ -> failwith "Recursive record wrong type")
    | Types.NAME (_, ty) -> (match !ty with None -> "None" | Some ty -> string_of_type ~verbose:verbose ty)
    | Types.UNDEFINED -> "!undefined!"

let string_of_envtype = function
    | Env.VarEntry {ty;_} -> "var of " ^ (string_of_type ty)
    | Env.FunEntry {formals;result;level;_} -> "fun of" ^ (List.fold_left (fun acc x -> acc ^ " " ^ (string_of_type x))
                                                        "" formals)
                                                      ^ " -> " ^ (string_of_type result) ^ ( if (Translate.requiresStaticLink level) then " (has sl)" else "")

let print_tenv ?(verbose=false) tenv =
    print_string "############# TENV ################\n";
    List.iter (fun (key, value) -> (Printf.printf "Key: %d => %s\n" key (string_of_type ~verbose:verbose ~show_recursive:true value)))
              (Symbol.to_list tenv);
    print_string "###################################\n"

let print_venv venv = 
    print_string "############# VENV ################\n";
    List.iter (fun (key, value) -> (Printf.printf "Key: %d => %s\n" key (string_of_envtype value)))
              (Symbol.to_list venv);
    print_string "###################################\n"


let actual_ty ?(pos=42069) (ty:Types.ty) = match ty with
    | Types.NAME (name, ty) -> begin match !ty with
        | None -> let _ = Errormsg.error pos ((Symbol.name name)^" DEVELOPER, FIX YOUR RECURSIVE TYPE CHECKING IMPLEMENTATION") in
                  Types.UNDEFINED
        | Some ty -> ty
    end
    | Types.REC_RECORD ty -> begin match !ty with
        | Types.RECORD (x,y,z) -> Types.RECORD (x,y,z) (*ocaml complains ?????*)
        | _ -> failwith "Recursive record not record type"
    end
    | _ -> ty

let checkInt ?(message="integer required") ({ty; _}:expty) pos =
    let ty = actual_ty ty in
    if ty = Types.INT || ty = Types.UNDEFINED then () else
        Errormsg.error pos message

let checkString ?(message="string required") ({ty; _}:expty) pos =
    let ty = actual_ty ty in
    if ty = Types.STRING || ty = Types.UNDEFINED then () else
        Errormsg.error pos message

let checkRecord ?(message="record required") ({ty; _}:expty) pos =
    let ty = actual_ty ty in match ty with
        Types.RECORD _ | Types.UNDEFINED -> ()
      | _ -> Errormsg.error pos message

let checkRecordOrNil ?(message="record required") unique ({ty; _}:expty) pos =
    let ty = actual_ty ty in match ty with
        Types.RECORD (_, _, u) when u == unique -> () | Types.NIL | Types.UNDEFINED -> ()
      | _ -> Errormsg.error pos message

let checkArray ?(message="array required") unique ({ty; _}:expty) pos =
    let ty = actual_ty ty in match ty with
        Types.ARRAY (_, _, u) when u == unique -> () | Types.UNDEFINED -> ()
      | _ -> Errormsg.error pos message

let checkUnit ?(message="unit required") ({ty; _}:expty) pos =
    let ty = actual_ty ty in
    if ty = Types.UNIT || ty = Types.UNDEFINED then () else
        Errormsg.error pos message

let errorIfName ({ty; _}:expty) pos = let _ = actual_ty ~pos:pos ty in ()

let compare_types expected actual_expty pos buildMessage (index:string) = 
    match actual_ty expected with
     | Types.UNDEFINED -> (*has its own error message*) ()
     | Types.INT -> checkInt ~message:(buildMessage index "int" (string_of_type actual_expty.ty)) actual_expty pos
     | Types.STRING -> checkString ~message:(buildMessage index "string" (string_of_type actual_expty.ty)) actual_expty pos
     | Types.RECORD (_, _, u) -> checkRecordOrNil ~message:(buildMessage index (string_of_type expected) (string_of_type actual_expty.ty)) u actual_expty pos
     | Types.ARRAY (_, _, u) -> checkArray ~message:(buildMessage index (string_of_type expected) (string_of_type actual_expty.ty)) u actual_expty pos
     | Types.UNIT -> checkUnit ~message:(buildMessage index "unit" (string_of_type actual_expty.ty)) actual_expty pos
     | _ -> Errormsg.error pos "the developer did a mistake and a function param can be apparently nil or Types.Name"

let rec convert_fields tenv ?(is_params=false) ?(seen=[]) (fields:A.field list) = match fields with
    | [] -> []
    | {name; typ; pos; escape; _} :: t ->
        let str = Symbol.name name in
        let exists = List.exists (fun x -> x = str) seen in
        let _ = if exists then Errormsg.error pos ("duplicate " ^ if is_params then "parameter" else "field" ^ " \"" ^ str ^ "\"") in
        let ty = match Symbol.look tenv typ with
            | None -> let _ = Errormsg.error pos ("type \"" ^ (Symbol.name typ) ^ "\" is not declared in this scope") in
                      Types.UNDEFINED
            | Some t -> t
        in
        if exists
        then convert_fields tenv ~is_params:is_params ~seen:seen t
        else (name, ty, !escape) :: convert_fields tenv ~is_params:is_params ~seen:(str::seen) t

and transExp ?(breakable=false) (venv: Env.enventry Symbol.table) tenv exp (l:Translate.level) : expty =
    let rec checkFields expected actual record_pos = match expected with
        | (e_sym, e_typ) :: rest -> begin
            match actual with
            | [] -> List.iter (fun (sym, typ) -> Errormsg.error record_pos ("field \"" ^
                                             (Symbol.name sym) ^ "\" of type \"" ^
                                             (string_of_type typ) ^ "\" is missing"))
                              expected
            | _ -> let (matches, remain) = List.partition (fun (sym, _, _) -> (Symbol.name sym) = (Symbol.name e_sym)) actual
                   in begin match matches with
                        | [] -> Errormsg.error record_pos ("field \"" ^
                                 (Symbol.name e_sym) ^ "\" of type \"" ^
                                 (string_of_type e_typ) ^ "\" is missing")
                        | [(_, expr, pos)] -> let check = (trExp expr) in
                            let buildMessage field expected received = "field \"" ^ field ^ "\" must be " ^ expected ^ ", but received " ^ received in
                            compare_types e_typ check pos buildMessage (Symbol.name e_sym);
                            checkFields rest remain record_pos

                        | _ -> Errormsg.error record_pos ("duplicate field \"" ^ (Symbol.name e_sym) ^ "\"")
                   end
        end
        | [] -> List.iter (fun (sym, _, pos) -> Errormsg.error pos
                                                ("record definition does not contain field \"" ^
                                                (Symbol.name sym) ^ "\" and so it must be removed"))
                          actual

    and transVar ?(must_be_mutable=false) var : expty = match var with
        | A.SimpleVar (sym, pos) ->
            let found_opt = Symbol.look venv sym in if found_opt = None then
                let _ = Errormsg.error pos ("variable \"" ^ (Symbol.name sym) ^ "\" is not declared in this scope") in
                {exp = (); ty = Types.UNDEFINED}
            else begin match Option.get found_opt with
                | Env.FunEntry _ -> let _ = Errormsg.error pos ("first order functions not allowed") in
                    {exp = (); ty = Types.UNDEFINED}
                | Env.VarEntry {ty;immutable;_} -> let _ = if must_be_mutable && immutable
                                                         then Errormsg.error pos ("variable " ^ (Symbol.name sym) ^ " is immutable")
                                                 in {exp = (); ty = ty}
            end

        | A.FieldVar (var, sym, pos) ->
            let {ty = var_ty; _} = transVar var in begin match var_ty with
                | Types.RECORD (fields, name, _) ->
                    let found = List.find_opt (fun (sym', _) -> (Symbol.name sym) = (Symbol.name sym'))
                                              fields
                    in begin match found with
                        | None -> let _ = Errormsg.error pos ("Record type \"" ^ name ^ "\" does not contain field \"" ^ (Symbol.name sym) ^ "\"") in
                                  {exp = (); ty = Types.UNDEFINED}
                        | Some (_, ty) -> {exp = (); ty = ty}
                    end
                | _ -> let _ = Errormsg.error pos "dot operator must be used on a record type" in
                       {exp = (); ty = Types.UNDEFINED}
            end
        | A.SubscriptVar (var, expr, pos) ->
            let {ty = var_ty; _} = transVar var in begin match var_ty with
                | Types.ARRAY (elem_ty, _, _) ->
                    let _ = checkInt (trExp expr) pos ~message:"subscript expression must be an integer" in
                    {exp = (); ty = elem_ty}
                | _ -> let _ = Errormsg.error pos "subscript operator must be used on an array type" in
                       {exp = (); ty = Types.UNDEFINED}
            end

    and trExp ?(breakable=false) expr : expty = match expr with
        | A.VarExp var -> transVar var
        | A.NilExp -> {exp = (); ty = Types.NIL}
        | A.IntExp _ -> {exp = (); ty = Types.INT}
        | A.StringExp _ -> {exp = (); ty = Types.STRING}
        | A.CallExp {func; args; pos} -> begin
            let sym_opt = Symbol.look venv func in if sym_opt = None then 
                let _ = Errormsg.error pos ("function/procedure \"" ^ (Symbol.name func) ^ "\" is not declared in this scope") in
                {exp = (); ty = Types.UNDEFINED}
            else match Option.get sym_opt with
                | Env.VarEntry _ -> let _ = Errormsg.error pos ("\"" ^ (Symbol.name func) ^ "\" is not a function/procedure") in
                    {exp = (); ty = Types.UNDEFINED}
                | Env.FunEntry {formals; result; _} ->
                let len_params = List.length formals in
                let len_args = List.length args in
                let typ = match result with Types.UNIT -> "procedure" | _ -> "function" in
                if len_params <> len_args then
                    let _ = Errormsg.error pos (typ ^ " expects " ^ (string_of_int len_params) ^ " arguments but got " ^ (string_of_int len_args)) in
                        {exp = (); ty = result}
                else 
                    let buildMessage num expected received = "argument " ^ num ^ " must be " ^ expected ^ ", but received " ^ received in
                    let rec checkArgs i = function
                        | (param :: pt, arg' :: at) -> let arg = (trExp arg')
                                in compare_types param arg pos buildMessage (string_of_int i);
                                   checkArgs (i+1) (pt, at)
                        | _ -> ()
                    in let _ = checkArgs 1 (formals, args) in {exp = (); ty = result}
            end
        | A.OpExp {left; oper; right; pos} -> begin
            match oper with
                | A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp
                    -> let _, _ = (checkInt (trExp left) pos), (checkInt (trExp right) pos) in
                        {exp = (); ty = Types.INT}
                | A.GeOp | A.LeOp | A.GtOp | A.LtOp
                    -> let l, r = (trExp left), (trExp right) in
                        if l.ty = Types.INT then let _ = checkInt r pos in {exp = (); ty = Types.INT} else
                        if l.ty = Types.STRING then let _ = checkString r pos in {exp = (); ty = Types.INT} else
                        let _ = Errormsg.error pos "integer or string required" in {exp = (); ty = Types.INT}
                | A.EqOp | A.NeqOp
                    -> let (l, r) = (trExp left), (trExp right) in
                        match actual_ty l.ty with
                          | Types.UNDEFINED -> (*It has its own error message*) {exp = (); ty = Types.INT}
                          | Types.INT -> let _ = checkInt r pos in {exp = (); ty = Types.INT}
                          | Types.STRING -> let _ = checkString r pos in {exp = (); ty = Types.INT}
                          | Types.RECORD (_, name, u) -> let _ = checkRecordOrNil ~message:(name ^ " record required") u r pos in {exp = (); ty = Types.INT}
                          | Types.ARRAY (_, name, u) -> let _ = checkArray ~message:(name ^ " array required") u r pos in {exp = (); ty = Types.INT}
                          | Types.NIL -> let _ = checkRecord r pos in {exp = (); ty = Types.INT}
                          | _ -> let _ = Errormsg.error pos "valueful expression required" in {exp = (); ty = Types.INT}
            end
        | A.RecordExp {fields; typ; pos} -> begin
            let sym_opt = Symbol.look tenv typ in if sym_opt = None then
                let _ = Errormsg.error pos ("record type \"" ^ (Symbol.name typ) ^ "\" is not declared in this scope") in
                {exp = (); ty = Types.UNDEFINED}
            else let record = Option.get sym_opt in match actual_ty record with
                | Types.RECORD (the_fields, _, _) -> let _ = checkFields the_fields fields pos in
                        {exp = (); ty = record}
                | _ -> let _ = Errormsg.error pos ("\"" ^ (Symbol.name typ) ^ "\" is not a record type") in
                        {exp = (); ty = Types.UNDEFINED}
        end
        | A.SeqExp (lst, pos) -> let rec iter nth = function
            | [] -> {exp = (); ty = Types.UNIT}
            | [h] -> trExp ~breakable:breakable h
            | h :: t -> let _ = errorIfName (trExp ~breakable:breakable h) pos
                        in iter (nth+1) t
            in iter 1 lst
        | A.AssignExp {var; expr; pos} -> let {ty; _} = (transVar ~must_be_mutable:true var) in
            let buildMessage _ expected received = "the assigned value must be " ^ expected ^ ", but received " ^ received in
            let _ = compare_types ty (trExp expr) pos buildMessage "" in {exp = (); ty = Types.UNIT}
        | A.IfExp {test; then'; else'; pos} ->
            let _ = checkInt (trExp test) pos ~message:"test condition must be an integer" in
            begin match else' with
                | None -> let _ = checkUnit (trExp ~breakable:breakable then') pos ~message:"\"then\" clause must be unit (no \"else\" clause)" in
                          {exp = (); ty = Types.UNIT}
                | Some els -> let {ty; _} = (trExp ~breakable:breakable then') in
                              let buildMessage _ expected received = "\"else\" clause's type must match \"then\" clause's type of " ^ expected ^ ", but received " ^ received in
                              let _ = compare_types ty (trExp ~breakable:breakable els) pos buildMessage "" in
                              {exp = (); ty = ty}
            end
        | A.WhileExp {test; body; pos} -> 
            let _ = checkInt (trExp test) pos ~message:"test condition must be an integer" in
            let _ = checkUnit (trExp ~breakable:true body) pos ~message:"body must be a valueless expression" in
            {exp = (); ty = Types.UNIT}
        | A.ForExp {var; escape; lo; hi; body; pos;} -> 
            let _ = checkInt (trExp lo) pos ~message:"lower bound must be an integer" in
            let _ = checkInt (trExp hi) pos ~message:"upper bound must be an integer" in

            let data:Translate.data_formal = {escape = !escape; elementary = true} in
            let _ = checkUnit
                        (transExp
                            ~breakable:true
                            (Symbol.enter venv var
                                (Env.VarEntry {ty = Types.INT;
                                               immutable = true;
                                               access = (Translate.allocLocal l data)}))
                            tenv body l)
                        pos
                        ~message:"body must be a valueless expression" in
            {exp = (); ty = Types.UNIT}
        | A.BreakExp pos -> let _ = if not breakable then Errormsg.error pos "\"break\" is illegal here" in
                            {exp = (); ty = Types.UNIT}
        | A.LetExp {decs; body; _} -> let (venv', tenv') = transDecs venv tenv decs l in
            (*let _ = print_tenv ~verbose:true tenv' in*)
            (*let _ = print_venv venv' in*)
            transExp ~breakable:breakable venv' tenv' body l
        | A.ArrayExp {typ; size; init; pos} -> begin
            let sym_opt = Symbol.look tenv typ in if sym_opt = None then
                let _ = Errormsg.error pos ("array type \"" ^ (Symbol.name typ) ^ "\" is not declared in this scope") in
                {exp = (); ty = Types.UNDEFINED}
            else let arr = Option.get sym_opt in match arr with
                | Types.ARRAY (elem_type, _, _) ->
                    let _ = checkInt (trExp size) pos ~message:"array size must be an integer" in
                    let buildMessage _ expected received = "initializer type must be " ^ expected ^ ", but received " ^ received in
                    let _ = compare_types elem_type (trExp init) pos buildMessage "" in
                    {exp = (); ty = arr}
                | _ -> let _ = Errormsg.error pos ("\"" ^ (Symbol.name typ) ^ "\" is not a array type") in
                        {exp = (); ty = Types.UNDEFINED}
        end

    in trExp ~breakable:breakable exp 

and transDecs venv tenv decs (l:Translate.level) = match decs with
    | [] -> (venv, tenv)
    (*A VarDec can overwrite a previous VarDec with the same name*)
    | A.VarDec {name; escape; typ; init; pos; _} :: t ->
        let init_expty = (transExp venv tenv init l) in
        let {ty = init_ty;_} = init_expty in
        begin match typ with
        | None -> if init_ty = Types.NIL
                then let _ = Errormsg.error pos "\"nil\" initiliazer requires declaration with explicit record type" in
                    transDecs (Symbol.enter venv name (Env.VarEntry {ty = Types.UNDEFINED; immutable = false; access = Translate.dummy_access})) tenv t l
                else
                    let data:Translate.data_formal = {escape= !escape; elementary=Types.is_elementary init_ty} in
                    transDecs (Symbol.enter venv name
                            (Env.VarEntry {ty=init_ty; immutable=false; access=Translate.allocLocal l data})) tenv t l
        | Some (sym, sym_pos) -> begin match Symbol.look tenv sym with
            | None -> let _ = Errormsg.error sym_pos ("type \"" ^ (Symbol.name sym) ^ "\" is not declared in this scope") in
                    transDecs (Symbol.enter venv name (Env.VarEntry {ty = Types.UNDEFINED; immutable = false; access = Translate.dummy_access})) tenv t l
            | Some actual_type -> 
                let buildMessage _ expected received = "initializer type must be " ^ expected ^ ", but received " ^ received in
                let _ = compare_types actual_type init_expty sym_pos buildMessage "" in
                let data:Translate.data_formal = {escape= !escape; elementary=Types.is_elementary actual_type} in
                transDecs (Symbol.enter venv name (Env.VarEntry {ty=actual_type; immutable=false; access=Translate.allocLocal l data})) tenv t l
            end
        end
    | (A.FunctionDec lst) :: t ->
        let rec filter_duplicates ?(seen=[]) lst = match lst with
            | [] -> []
            | h :: t ->
                let ({name;pos;_}:A.fundec) = h in
                if List.exists (fun x -> (x = (Symbol.name name))) seen
                then let _ = Errormsg.error pos ("function/procedure \"" ^ (Symbol.name name) ^ "\" is already declared in this scope") in
                     filter_duplicates ~seen:seen t
                else h :: filter_duplicates ~seen:((Symbol.name name)::seen) t

        in let rec scan_headers ?(venv=venv) ?(data=[]) (fundecs:A.fundec list) = match fundecs with
            | [] -> (venv, (List.rev data))
            | {name;result;params;static_link;_} :: t ->
                let params = convert_fields tenv ~is_params:true params
                in let (_, params_ty, params_esc) = list3split params
                in let ty = begin match result with
                    | None -> Types.UNIT
                    | Some (sym, sym_pos) -> begin match Symbol.look tenv sym with
                        | None ->
                            let _ = Errormsg.error sym_pos ("type \"" ^ (Symbol.name sym) ^ "\" is not declared in this scope") in
                                    Types.UNDEFINED
                        | Some ty -> ty
                        end
                    end
                in let arg_data = List.map (fun (ty, esc) -> ({escape=esc;elementary=Types.is_elementary ty}:Translate.data_formal))
                                                        (List.combine params_ty params_esc)
                in let label = Temp.newLabel()
                in let args:Translate.newLevelArgs = {parent=l;name=label;
                                                      needs_static_link= !static_link;formals=arg_data}
                in let level = Translate.newLevel args in
                scan_headers
                    ~venv:(Symbol.enter venv name
                        (Env.FunEntry {formals=params_ty;result=ty;level=level;label=label}))
                    ~data:((params,ty,level)::data)
                    t

        in let lst = filter_duplicates lst
        in let (new_venv, data) = scan_headers lst (*we assume data is in the same order*)

        in let rec scan_bodies (fundecs:A.fundec list) data = match fundecs with
            | [] -> ()
            | {body;pos;_} :: t ->
                let (params, result, new_level) = (List.hd data) in
                let in_venv = List.fold_left
                    (fun acc_venv ((sym, ty, _), access) ->
                        Symbol.enter acc_venv sym
                                    (Env.VarEntry {ty=ty; immutable = false; access = access}))
                    new_venv
                    (* big assumption everything is in the right order! :) *)
                    (List.combine params (Translate.formals new_level))
                in let buildMessage _ expected received = "body must be " ^ expected ^ ", but received " ^ received
                in compare_types result (transExp in_venv tenv body new_level) pos buildMessage "";
                   scan_bodies t (List.tl data)

        in let _ = scan_bodies lst data in transDecs new_venv tenv t l

    | (A.TypeDec lst) :: t ->
        let rec filter_duplicates ?(seen=[]) lst = match lst with
            | [] -> []
            | h :: t ->
                let ({name;pos;_}:A.typeDecRecord) = h in
                if List.exists (fun x -> (x = (Symbol.name name))) seen
                then let _ = Errormsg.error pos ("type \"" ^ (Symbol.name name) ^ "\" is already declared in this scope") in
                     filter_duplicates ~seen:seen t
                else h :: filter_duplicates ~seen:((Symbol.name name)::seen) t

        in let rec scan_headers ?(tenv=tenv) (tydecs:A.typeDecRecord list) = match tydecs with
            | [] -> tenv
            | {name;_} :: t -> scan_headers
                    ~tenv:(Symbol.enter tenv name (Types.NAME (name, ref None)))
                    t

        (*To simplify loop checking*)
        in let rec resolveRecordsInTenvButDontProcessThem tenv (record_tydecs:A.typeDecRecord list) = match record_tydecs with
            | [] -> ()
            | {name;ty;_} :: t -> begin match ty with
                | A.RecordTy _ -> begin match Option.get (Symbol.look tenv name) with
                    | Types.NAME (_, ty) -> ty := Some (Types.RECORD ([], "Placeholder", placeholder_unique));
                                            resolveRecordsInTenvButDontProcessThem tenv t
                    | _ -> failwith "Got an external type, but expecting a local unresolved type"
                end
                | _ -> failwith "Separate the records from the rest first before calling this function. It's for efficiency."
            end

        (*After a bit of testing, it turns out THAT I 100% FIRST-TRIED IT*)
        in let resolveLoops tenv (not_record_tydecs:A.typeDecRecord list) = 
            let rec resolve ?(seen=[]) tenv (tydec:A.typeDecRecord) =
                let seen = (Symbol.name tydec.name) :: seen in
                let (sym:Types.ty) = Option.get (Symbol.look tenv tydec.name) in
                match sym with
                    | Types.NAME (_, ty) -> begin match !ty with
                        | Some _ -> (tenv, false)
                        | None -> begin match tydec.ty with
                            | A.RecordTy _ -> failwith "Tydecs have records and I want the algo to be optimized, so please use the function correctly."
                            | A.NameTy (ty_sym, _)
                            | A.ArrayTy (ty_sym, _) ->
                                let pointed_tydec = (List.find_opt (fun (x:A.typeDecRecord) -> (Symbol.name x.name) = (Symbol.name ty_sym)) not_record_tydecs) in
                                match pointed_tydec with
                                    | None -> (tenv, false)
                                    | Some pointed_tydec ->
                                if List.exists (fun x -> x = (Symbol.name pointed_tydec.name)) seen
                                then let _ = Errormsg.error tydec.pos ("Illegal loop " ^ string_of_cycle (List.drop_while (fun x -> x <> (Symbol.name ty_sym)) (List.rev seen))) in
                                     ((Symbol.enter tenv tydec.name Types.UNDEFINED), true)
                                else let (tenv', is_loop) = resolve tenv
                                                                    pointed_tydec
                                                                    ~seen:seen
                                     in if is_loop
                                        then ((Symbol.enter tenv' tydec.name Types.UNDEFINED), true)
                                        else (tenv', false)
                            end
                        end
                    | _ -> (tenv, false)

            in let rec iter tenv tydecs = match tydecs with
                | [] -> tenv
                | tydec :: rest -> let (tenv', _) = resolve tenv tydec
                                   in iter tenv' rest
            in iter tenv not_record_tydecs

        (*After a bit of testing, it turns out THAT I 95% FIRST-TRIED IT*)
        in let resolveEverything tenv (tydecs:A.typeDecRecord list) =
            let in_tydecs_fun searched_sym = (fun (x:A.typeDecRecord) -> Symbol.name (x.name) = Symbol.name searched_sym) in
            let rec resolve ?(seen=[]) tenv tydec =
                let {name;ty;_}:A.typeDecRecord = tydec in

                (*let _ = print_string ((Symbol.name name) ^ " "); List.iter (fun x -> print_string x) (List.rev seen) ;print_newline () in*)
                let (in_tenv_ty:Types.ty) = Option.get (Symbol.look tenv name) in

                (*if record is already seen*)
                if List.exists (fun x -> x = (Symbol.name name)) seen
                then
                    (*Isolate loop*)
                    let loop = List.drop_while (fun x -> x <> (Symbol.name name)) (List.rev seen) in

                    (*let _ = print_string ("  " ^ (string_of_cycle loop) ^ " ") in*)
                    (*let _ = print_int (List.length loop); print_newline () in*)

                    (tenv, in_tenv_ty, (List.length loop)) (*if the record is already seen, it's in tenv (check algo below)*)
                else

                match in_tenv_ty with
                Types.NAME (_, _) -> begin
                    match ty with
                        | A.NameTy (sym, _) ->
                            begin match List.find_opt (in_tydecs_fun sym) tydecs with
                                | None ->
                                    let transed = (transTy ~type_name:(Symbol.name name) tenv ty) in
                                    ((Symbol.enter tenv name transed), transed, 0)
                                | Some found_tydec ->
                                    let (tenv', resolved, rec_cnt) = resolve tenv found_tydec ~seen:seen in
                                    ((Symbol.enter tenv' name resolved), resolved, rec_cnt)
                            end
                        | A.ArrayTy (sym, _) ->
                            begin match List.find_opt (in_tydecs_fun sym) tydecs with
                                | None ->
                                    let transed = (transTy ~type_name:(Symbol.name name) tenv ty) in
                                    ((Symbol.enter tenv name transed), transed, 0)
                                | Some found_tydec ->
                                    let (tenv', resolved, rec_cnt) = resolve tenv found_tydec ~seen:seen in
                                    let arrayed = Types.ARRAY (resolved, (Symbol.name name), ref ()) in
                                    ((Symbol.enter tenv' name arrayed), arrayed, rec_cnt)
                            end
                        | A.RecordTy fields ->
                            let tydecky = List.filter_map (fun (x:A.field) -> (List.find_opt (in_tydecs_fun x.typ) tydecs)) fields in

                            (*simple optimization, premature and unnecessary even, BUT IT'S MY CODE*)
                            if List.is_empty tydecky
                            then let t = transTy ~type_name:(Symbol.name name) tenv ty in
                                 ((Symbol.enter tenv name t), t, 0)
                            else

                            (*assume it's recursive (so we can resolve self(ish)-refs)*)
                            let rec_ref = ref placeholder_rec in
                            let tenv = Symbol.enter tenv name (Types.REC_RECORD rec_ref) in

                            let seen = (Symbol.name name) :: seen in
                            let (tenv', rec_left) = List.fold_left (fun (tenv, recs) x -> (
                                let (tenv', _, recs_result) = resolve tenv x ~seen:seen in
                                (tenv', (max recs recs_result))
                            )) (tenv, 0) tydecky in

                            let the_record = transTy ~type_name:(Symbol.name name) tenv' ty in
                            if rec_left > 0
                                then let _ = rec_ref := the_record in
                                     (tenv', (Types.REC_RECORD rec_ref), (rec_left - 1))
                                else ((Symbol.enter tenv' name the_record), the_record, 0)
                    end
                | x -> (tenv, x, 0)

            in let rec iter tenv = function
                | [] -> tenv
                | tydec :: rest -> let (tenv', _, _) = resolve tenv tydec in
                            iter tenv' rest
            in iter tenv tydecs

        in let lst = filter_duplicates lst
        in let new_tenv = scan_headers lst

        in let (records, rest) = List.partition (fun x -> match (x:A.typeDecRecord).ty with A.RecordTy _ -> true | _ -> false) lst

        (* "identify illegal cycles" section *)
        in let _ = resolveRecordsInTenvButDontProcessThem new_tenv records
        in let tweaked_tenv = resolveLoops new_tenv rest

        (* actual resolving finally*)
        in let final_tenv = resolveEverything tweaked_tenv lst
        in transDecs venv final_tenv t l

and transTy ?(type_name="The developer forgor something") tenv ty  = match ty with
    | A.NameTy (sym, pos) -> begin match Symbol.look tenv sym with
        | None -> let _ = Errormsg.error pos ("type \"" ^ (Symbol.name sym) ^ "\" is not declared in this scope") in
                  Types.UNDEFINED
        | Some ty -> ty
    end
    | A.RecordTy fields ->
        Types.RECORD ((List.map (fun (f,s,_) -> (f,s)) (convert_fields tenv fields)), type_name, ref ())
    | A.ArrayTy (sym, pos) -> begin match Symbol.look tenv sym with
        | None -> let _ = Errormsg.error pos ("type \"" ^ (Symbol.name sym) ^ "\" is not declared in this scope") in
                  Types.UNDEFINED
        | Some ty -> Types.ARRAY (ty, type_name, ref ())
    end

let transProg expr = 
    let () = FindEscape.findEscape expr in
    (transExp Env.base_venv Env.base_tenv expr (Translate.outermost))
