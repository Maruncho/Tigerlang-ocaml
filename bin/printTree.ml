
module T = Tree

let printTree (outstream:out_channel) (s0:Tree.stm) =
    let say s = Out_channel.output_string outstream s in
    let sayln s = say (s^"\n") in
    let indent n = say (String.make n '-') in

    let rec stm s d = indent d; match s with
        | T.SEQ (a, b) -> sayln "SEQ("; stm a (d+1); sayln ","; stm b (d+1); say ")"
        | T.LABEL lab -> say "LABEL "; say (Symbol.name lab)
        | T.JUMP (e,_) -> sayln "JUMP("; exp e (d+1); say ")"
        | T.CJUMP (r,t,f) -> sayln "CJUMP("; exp r (d+1); sayln ","; indent (d+1);
                             say ((Symbol.name t)^", "^(Symbol.name f)^")")
        | T.MOVE (a, b) -> sayln "MOVE("; exp a (d+1); sayln ","; exp b (d+1); say ")"
        | T.EXP e -> sayln "EXP("; exp e (d+1); say ")"

    and exp e d = indent d; match e with
        | T.BINOP (p,a,b) -> say "BINOP("; binop p; sayln ","; exp a (d+1); sayln ","; exp b (d+1); say ")"
        | T.RELOP (p,a,b) -> say "RELOP("; relop p; sayln ","; exp a (d+1); sayln ","; exp b (d+1); say ")"
        | T.MEM e -> sayln "MEM("; exp e (d+1); say ")"
        (*| T.MEM_FAT (e, s) -> sayln "MEM_FAT("; exp e (d+1); exp s (d+1); say ")"*)
        | T.TEMP t -> say ("TEMP "^(Temp.makeString t))
        | T.ESEQ (s, e) -> sayln "ESEQ("; stm s (d+1); sayln ","; exp e (d+1); say ")"
        | T.NAME lab -> say "NAME "; say (Symbol.name lab)
        | T.CONST i -> say "CONST "; say (string_of_int i)
        | T.MAXINT -> say "CONST "; say (Int64.to_string Int64.max_int)
        | T.CALL (e, el) -> sayln "CALL("; exp e (d+1); List.iter (fun a -> (sayln ","; exp a (d+2))) el; say ")"
        | T.NEG e -> sayln "NEG("; exp e (d+1); say ")"
        | T.UNDEFINED -> say "!!UNDEFINED!!"

    and binop = function
        | T.PLUS -> say "PLUS"
        | T.MINUS -> say "MINUS"
        | T.MUL -> say "MUL"
        | T.DIV -> say "DIV"
        | T.AND -> say "AND"
        | T.OR -> say "OR"
        | T.LSHIFT -> say "LSHIFT"
        | T.RSHIFT -> say "RSHIFT"
        | T.ARSHIFT -> say "ARSHIFT"
        | T.XOR -> say "XOR"

    and relop = function
        | T.EQ -> say "EQ"
        | T.NE -> say "NE"
        | T.LT -> say "LT"
        | T.GT -> say "GT"
        | T.LE -> say "LE"
        | T.GE -> say "GE"
        | T.ULT -> say "ULT"
        | T.ULE -> say "ULE"
        | T.UGT -> say "UGT"
        | T.UGE -> say "UGE"

    in stm s0 0; sayln ""; Out_channel.flush outstream
