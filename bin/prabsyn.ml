
type pos = int
and symbol = Symbol.symbol

module A = Absyn

let print outchannel e0 =
    let rec say s = output_string outchannel s
    and sayln s = (say s; say "\n")
    and indent = function 0 -> () | i -> (say " "; indent (i-1))
    and opname = function
        | A.PlusOp -> "PlusOp"
        | A.MinusOp -> "MinusOp"
        | A.TimesOp -> "TimesOp"
        | A.DivideOp -> "DivideOp"
        | A.EqOp -> "EqOp"
        | A.NeqOp -> "NeqOp"
        | A.LtOp -> "LtOp"
        | A.LeOp -> "LeOp"
        | A.GtOp -> "GtOp"
        | A.GeOp -> "GeOp"

    in let rec dolist d f = function
        | [h] -> (sayln ""; f h (d+1))
        | h :: t -> (sayln ""; f h (d+1); say ","; dolist d f t)
        | [] -> ()

    in let rec var varVariant d = match varVariant with
        | A.SimpleVar(s, _) -> (indent d; say "SimpleVar(";
                                          say(Symbol.name s); say ")")
        | A.FieldVar(v, s, _) -> (indent d; sayln "FieldVar(";
                                  var v (d+1); sayln ",";
                                  indent (d+1); say(Symbol.name s); say ")")
        | A.SubscriptVar(v, e, _) -> (indent d; sayln "SubscriptVar(";
                                      var v (d+1); sayln ",";
                                      expr e (d+1); say ")")

    and expr expVariant d = match expVariant with
        | A.VarExp v -> (indent d; sayln "VarExp("; var v (d+1); say ")")
        | A.NilExp -> (indent d; say "NilExp")
        | A.IntExp i -> (indent d; say "IntExp("; say (string_of_int i); say ")")
        | A.StringExp(s, _) -> (indent d; say "StringExp(\"";
                                say s; say "\")")
        | A.CallExp {func; args; _} -> (indent d; say "CallExp("; say (Symbol.name func);
                                          say ",["; dolist d expr args; say "])")
        | A.OpExp {left; oper; right; _} -> (indent d; say "OpExp("; say (opname oper); sayln ",";
                                               expr left (d+1); sayln ","; expr right (d+1); say ")")
        | A.RecordExp {fields; typ; _} -> begin
            let f (name, e, _) d = 
                (indent d; say "("; say(Symbol.name name);
                 sayln ","; expr e (d+1);
                 say ")")
            in indent d; say "RecordExp("; say (Symbol.name typ);
               sayln ",["; dolist d f fields; say "])"
          end
        | A.SeqExp l -> (indent d; say "SeqExp["; dolist d expr (fst l);
                         say "]")
        | A.AssignExp {var=v; expr=e; _} ->
            (indent d; sayln "AssignExp("; var v (d+1); sayln ",";
             expr e (d+1); say ")")
        | A.IfExp {test; then'; else'; _} ->
            (indent d; sayln "IfExp("; expr test (d+1); sayln ",";
             expr then' (d+1);
             match else' with None -> ()
                            | Some e -> (sayln ","; expr e (d+1));
             say ")")
        | A.WhileExp {test; body; _} ->
            (indent d; sayln "WhileExp("; expr test (d+1); sayln ",";
             expr body (d+1); say ")")
        | A.ForExp {var=v; escape=b; lo; hi; body; _} ->
            (indent d; sayln "ForExp(";
             say (Symbol.name v); say ","; say (string_of_bool !b); sayln ",";
             expr lo (d+1); sayln ","; expr hi (d+1); sayln ",";
             expr body (d+1); say ")")
        | A.BreakExp _ -> (indent d; say "BreakExp")
        | A.LetExp {decs; body; _} ->
            (indent d; say "LetExp([";
             dolist d dec decs; sayln "],"; expr body (d+1); say ")")
        | A.ArrayExp {typ; size; init; _} ->
            (indent d; say "ArrayExp("; say (Symbol.name typ); sayln ",";
             expr size (d+1); sayln ","; expr init (d+1); say ")")

    and dec (decVariant: A.dec) d = match decVariant with
        | A.FunctionDec l -> begin
            let rec field ({name; escape; typ; _} : A.field) d =
                (indent d; say "("; say (Symbol.name name);
                 say ","; say (string_of_bool !escape);
                 say ","; say (Symbol.name typ); say ")")
            and f ({name; params; result; body; _} : A.fundec) d =
                (indent d; say "("; say (Symbol.name name); say ", [";
                 dolist d field params; sayln "],";
                 match result with
                     None -> say "NONE"
                   | Some (s, _) -> (say "SOME("; say (Symbol.name s); say ")");
                 sayln ","; expr body (d+1); say ")")
            in indent d; say "FunctionDec["; dolist d f l; say "]"
          end
        | A.VarDec {name; escape; typ; init; _} ->
                (indent d; say "VarDec("; say (Symbol.name name); say ",";
                 say (string_of_bool !escape); say ",";
                 match typ with None -> say "NONE"
                       | Some (s, _) -> say "SOME("; say (Symbol.name s); say ")";
                 sayln ","; expr init (d+1); say ")")
        | A.TypeDec l -> begin
            let tdec ({name; ty=t; _}: A.typeDecRecord) d = (indent d; say "(";
                say (Symbol.name name); sayln ",";
                ty t (d+1); say ")")
            in indent d; say "TypeDec["; dolist d tdec l; say "]"
          end

    and ty tyVariant d = match tyVariant with
        | A.NameTy (s, _) -> (indent d; say "NameTy("; say (Symbol.name s); say ")")
        | A.RecordTy l -> begin
            let f ({name; escape; typ; _} : A.field) d =
                (indent d; say "("; say (Symbol.name name);
                 say ","; say (string_of_bool !escape); say ",";
                 say (Symbol.name typ); say ")")
            in indent d; say "RecordTy["; dolist d f l; say "]"
          end
        | A.ArrayTy (s, _) -> (indent d; say "ArrayTy("; say (Symbol.name s); say ")")

        in (expr e0 0); sayln ""; flush outchannel


