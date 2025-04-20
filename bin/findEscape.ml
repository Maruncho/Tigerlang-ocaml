
type depth = int
type escEnv = (depth * bool ref) Symbol.table

(*gives a "need static_link" status to every function below and excluding the variable's function*)
let rec require_static_chain (frames: bool ref list) num_up_the_chain =
    match (frames, num_up_the_chain) with
        | [], 0 -> ()
        | [], _ -> failwith "The static chain algo doensn't work!"
        | _, 0 -> ()
        | (h::t), _ -> h := true; require_static_chain t (num_up_the_chain - 1)

let rec traverseVar (env:escEnv) (d:depth) (frames: bool ref list) (s: Absyn.var) : unit = match s with
    | Absyn.SimpleVar (sym, _) -> begin match Symbol.look env sym with
        | None -> ()
        | Some (d_var, esc) -> if d > d_var then (esc := true; require_static_chain frames (d - d_var))
    end
    | Absyn.SubscriptVar (var, expr, _) -> traverseExpr env d frames expr; traverseVar env d frames var
    | Absyn.FieldVar (var, _, _) -> traverseVar env d frames var

and traverseExpr (env:escEnv) (d:depth) (frames: bool ref list) (s: Absyn.expr) : unit = match s with
    | Absyn.VarExp var -> traverseVar env d frames var
    | Absyn.NilExp -> () | Absyn.IntExp _ -> () | Absyn.StringExp _ -> ()
    | Absyn.CallExp {args;_} -> List.iter (fun x -> traverseExpr env d frames x) args
    | Absyn.OpExp {left;right;_} -> traverseExpr env d frames left; traverseExpr env d frames right
    | Absyn.RecordExp {fields;_} -> List.iter (fun (_, expr, _) -> traverseExpr env d frames expr) fields
    | Absyn.SeqExp (exprs, _) -> List.iter (fun x -> traverseExpr env d frames x) exprs
    | Absyn.AssignExp {var;expr;_} -> traverseVar env d frames var; traverseExpr env d frames expr
    | Absyn.IfExp {test;then';else';_} -> traverseExpr env d frames test; traverseExpr env d frames then';
                     (match else' with None -> () | Some else' -> traverseExpr env d frames else')
    | Absyn.WhileExp {test;body;_} -> traverseExpr env d frames test; traverseExpr env d frames body
    | Absyn.ForExp {var;escape;lo;hi;body;_} -> traverseExpr env d frames lo; traverseExpr env d frames hi;
                                    traverseExpr (Symbol.enter env var (d, escape)) d frames body
    | Absyn.BreakExp _ -> ()
    | Absyn.LetExp {decs;body;_} -> let env' = traverseDecs env d frames decs in traverseExpr env' d frames body
    | Absyn.ArrayExp {size;init;_} -> traverseExpr env d frames size; traverseExpr env d frames init


and traverseDecs (env:escEnv) (d:depth) (frames: bool ref list) (s: Absyn.dec list) : escEnv = match s with
    | [] -> env
    | h :: t -> begin match h with
        | Absyn.FunctionDec lst -> let () = List.iter (fun ({params;body;static_link;_}:Absyn.fundec) -> (
            let env' = List.fold_left (fun acc_env ({name;escape;_}:Absyn.field) ->
                (Symbol.enter acc_env name ((d+1), escape)))
            env params in traverseExpr env' (d+1) (static_link::frames) body
        )) lst in traverseDecs env d frames t
        | Absyn.VarDec {name;escape;init;_} -> let () = traverseExpr env d frames init in
                                               traverseDecs (Symbol.enter env name (d, escape)) d frames t
        | Absyn.TypeDec _ -> traverseDecs env d frames t
    end

let findEscape prog = traverseExpr (Symbol.empty) 0 [] prog
