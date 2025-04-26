
type level = Function of {this: Frame.frame; parent: level; unique: unit ref}
           | Outermost of {vframe: Frame.frame; stdlib: Temp.label list}
type access = level * Frame.access

type exp = Ex of Tree.exp
         | Nx of Tree.stm

let outermost = Outermost {vframe=Frame.newFrame (Symbol.symbol "<toplevel>") false []; stdlib=[]}
let dummy_access = (outermost, Frame.dummy_access)
let dummy_exp = Ex (Tree.UNDEFINED)

let out_of_bounds = Temp.namedLabel "subscript_out_of_bounds"
let nil_ptr_deref = Temp.namedLabel "null_pointer_dereference"

let std_stringCompare = "stringCompare"
let std_malloc = "malloc"
let std_initArray = "initArray"
let std_initArrayCopy = "initArrayCopy"

(* reversed order. It's for efficiency, don't wanna append big lists *)
let fragList:(Frame.frag list ref) = ref []

let getResult () = !fragList


type newLevelArgs = {parent: level; name: Temp.label; needs_static_link: bool; formals: bool list}

let newLevel ({parent: level; name: Temp.label; needs_static_link: bool; formals: bool list}:newLevelArgs) =
    Function {this=(Frame.newFrame name needs_static_link formals); parent=parent; unique=ref()}

let formals level = match level with
    | Outermost _ -> []
    | Function {this;_} ->
        List.map (fun fml -> (level, fml)) (Frame.formals this)

let allocLocal level escape = match level with
    | Outermost {vframe;_} -> (level, Frame.allocLocal vframe escape)
    | Function {this;_} -> (level, Frame.allocLocal this escape)

let requiresStaticLink = function
    | Outermost _ -> false
    | Function {this;_} -> Frame.requiresStaticLink this

let unEx = function
    | Ex e -> e
    | Nx stm -> Tree.ESEQ (stm, Tree.CONST 0)

let unNx = function
    | Ex e -> Tree.EXP e
    | Nx e -> e

let procEntryExit level body =
    let body_stm, frame = match level with
        | Outermost _ -> failwith "Outermost is not a function"
        | Function f -> (Frame.procEntryExit1 f.this (unNx body)), f.this

    in fragList := (Frame.PROC ({body=body_stm; frame=frame})) :: !fragList


let rec list_to_stm_seq = function
    | [] -> failwith "At least one element, please!"
    | [x] -> x
    | [x;y] -> Tree.SEQ (x,y)
    | x :: t -> Tree.SEQ (x, (list_to_stm_seq t))

let intExp num = Ex (Tree.CONST num)
let nilExp () = Ex (Tree.CONST 0)
let unitExp () = Nx (Tree.EXP (Tree.CONST 0))
let stringExp str =
    let lbl = Temp.newLabel() in
    let () = fragList := (Frame.STRING (lbl, str)) :: !fragList in
    Ex (Tree.NAME lbl)

let recordExp exprs =
    let size = List.length exprs in
    let r = Temp.newTemp() ~name:(Some "record ptr") in

    let rec loop i = function
        | [] -> []
        | e :: t -> Tree.MOVE (
                        Tree.MEM (Tree.BINOP (
                            Tree.PLUS,
                            Tree.TEMP r,
                            Tree.CONST (i * Frame.wordSize))),
                    unEx e) :: loop (i+1) t

    in Ex (Tree.ESEQ (
            Tree.SEQ (
                Tree.MOVE (
                    Tree.TEMP r,
                    Frame.externalCall std_malloc [Tree.CONST ((size+1) * Frame.wordSize)]),
                (list_to_stm_seq ((Tree.MOVE (Tree.MEM (Tree.TEMP r), Tree.CONST size)) :: (loop 1 exprs)))
        ), Tree.TEMP r))

let arrayExp size init cpy_seman =
    if cpy_seman
    then Ex (Frame.externalCall std_initArrayCopy [(unEx size); (unEx init)])
    else Ex (Frame.externalCall std_initArray [(unEx size); (unEx init)])

let seqExp prev last =
    let prev = List.map (fun x -> unNx x) prev in
    if List.is_empty prev
        then last
        else Ex (Tree.ESEQ (list_to_stm_seq prev, unEx last))

let simpleVar ((curLevel, frAccess):access) (parentLevel:level) =
    let fp = Tree.TEMP Frame.fp in
    let rec iter curLevel = match curLevel with
        | Outermost _ -> fp
        | Function {unique;_} ->
            let (parent, curFunUnique) = (match curLevel with Outermost _ -> failwith "iter Impossible Outermost" | Function f -> (f.parent, f.unique)) in
            if unique == curFunUnique
            then fp
            else Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.CONST (Frame.sl_offset), (iter parent)))
    in Ex (match parentLevel with
        | Outermost _ -> Frame.exp frAccess fp
        | Function {unique;_} ->
            let curFunUnique = (match curLevel with Outermost _ -> failwith "Impossible Outermost" | Function f -> f.unique) in
            if unique == curFunUnique
            then Frame.exp frAccess fp
            else Frame.exp frAccess (iter curLevel))

let subscript base index = 
    (Tree.MEM
        (Tree.BINOP
            (Tree.PLUS,
            (unEx base),
            (Tree.BINOP
                (Tree.MUL,
                (unEx index),
                (Tree.CONST Frame.wordSize))))))

let subscriptVar base index =
    (* INFO: std_initArray returns 8byte int arr_size + the array itself*)
    let to_snd_check, passed = (Temp.newLabel(), Temp.newLabel()) in
    Ex (Tree.ESEQ ((list_to_stm_seq (
        [Tree.CJUMP ((Tree.RELOP (Tree.LT, unEx index, (Tree.CONST 0))), out_of_bounds, to_snd_check);
         Tree.LABEL to_snd_check;
         Tree.CJUMP ((Tree.RELOP (Tree.GE, unEx index, unEx base), out_of_bounds, passed));
         Tree.LABEL passed])
    ),
        (subscript base (Ex (Tree.BINOP (Tree.PLUS, unEx index, Tree.CONST 1))))
    ))


let fieldVar base index =
    (* INFO: the record ptr is 8byte int rec_size + the record itself*)
    let passed = Temp.newLabel() in
    Ex (Tree.ESEQ ((list_to_stm_seq ([
         Tree.CJUMP ((Tree.RELOP (Tree.EQ, unEx base, (Tree.CONST 0))), nil_ptr_deref, passed);
         Tree.LABEL passed])
    ),
        (subscript base (Ex (Tree.CONST (index+1))))
    ))

let operArith = function
    | Absyn.PlusOp -> Tree.PLUS
    | Absyn.MinusOp -> Tree.MINUS
    | Absyn.TimesOp -> Tree.MUL
    | Absyn.DivideOp -> Tree.DIV
    | _ -> failwith "Use operComp"

let operComp = function
    | Absyn.EqOp -> Tree.EQ
    | Absyn.NeqOp -> Tree.NE
    | Absyn.LtOp -> Tree.LT
    | Absyn.LeOp -> Tree.LE
    | Absyn.GtOp -> Tree.GT
    | Absyn.GeOp -> Tree.GE
    | _ -> failwith "Use operArith"

let arith left right oper =
    (* optimization is in later stages *)
    Ex (Tree.BINOP (oper, (unEx left), (unEx right)))

let comp left right oper =
    (* optimization is in later stages *)
    Ex (Tree.RELOP (oper, (unEx left), (unEx right)))

let stringComp left right oper =
    let result = Frame.externalCall std_stringCompare [(unEx left); (unEx right)] in
    Ex (Tree.RELOP (oper, result, (Tree.CONST 0)))

let assignExp var expr =
    Nx (Tree.MOVE (unEx var, unEx expr))

let callExp args (callee_level:level) fun_lbl =
    let args = List.map (fun x -> unEx x) args in
    let args = if requiresStaticLink callee_level
               then
                   (Tree.BINOP (Tree.PLUS, (Tree.TEMP Frame.fp), Tree.CONST Frame.sl_offset))
                   :: args
                else args
    in Ex (Tree.CALL (Tree.NAME fun_lbl, args))

let ifExp test then' else' = match then' with
    | Ex _ -> begin 
        let r = Temp.newTemp() ~name:(Some "if result") in
        let (t, f, d) = (Temp.newLabel(), Temp.newLabel(), Temp.newLabel()) in
        Ex (Tree.ESEQ ((list_to_stm_seq (
            [Tree.CJUMP ((unEx test), t, f);
             Tree.LABEL f] @
            (match else' with
                None -> []
              | Some e -> [Tree.MOVE (Tree.TEMP r, (unEx e))]) @
            [Tree.JUMP (Tree.NAME d, [d]);
             Tree.LABEL t;
             Tree.MOVE (Tree.TEMP r, (unEx then'));
             Tree.LABEL d])
        ),
            Tree.TEMP r))
    end
    | Nx _ -> begin
        let (t, f, d) = (Temp.newLabel(), Temp.newLabel(), Temp.newLabel()) in
        Nx (list_to_stm_seq (
            [Tree.CJUMP ((unEx test), t, f);
             Tree.LABEL f] @
            (match else' with
                None -> []
              | Some e -> [unNx e]) @
            [Tree.JUMP (Tree.NAME d, [d]);
             Tree.LABEL t;
             unNx then';
             Tree.LABEL d])
        )
    end

let letExp decs body =
    let decs = List.map (fun x -> unNx x) decs in
    if List.is_empty decs
    then body
    else Ex (Tree.ESEQ (list_to_stm_seq decs, unEx body))

let whileExp test body done_lbl =
    let test_lbl = Temp.newLabel() in
    let body_lbl = Temp.newLabel() in
    Nx (list_to_stm_seq [
        Tree.LABEL test_lbl;
        Tree.CJUMP (Tree.NEG (unEx test), done_lbl, body_lbl);
        Tree.LABEL body_lbl;
        unNx body;
        Tree.JUMP (Tree.NAME test_lbl, [test_lbl]);
        Tree.LABEL done_lbl]
    )

let forExp lo hi body (_, iAccess) done_lbl =
    let fp = Tree.TEMP Frame.fp in
    let i = Frame.exp iAccess fp in
    let lim = Tree.TEMP (Temp.newTemp() ~name:(Some "forExp lim")) in
    let test = Ex (Tree.RELOP (Tree.LE, i, lim)) in
    let incr_lbl = Temp.newLabel() in
    let incr = list_to_stm_seq [
        Tree.CJUMP (Tree.RELOP (Tree.EQ, i, Tree.MAXINT), done_lbl, incr_lbl);
        Tree.LABEL incr_lbl;
        Tree.MOVE (i, Tree.BINOP (Tree.PLUS, i, Tree.CONST 1));
    ] in
    Nx (list_to_stm_seq [
        Tree.MOVE (i, unEx lo);
        Tree.MOVE (lim, unEx hi);
        unNx (whileExp test (Nx (Tree.SEQ (unNx body, incr))) done_lbl)]
    )

let breakExp done_lbl =
    Nx (Tree.JUMP (Tree.NAME done_lbl, [done_lbl]))

let varDec ((_, varAccess):access) init =
    let var = Frame.exp varAccess (Tree.TEMP Frame.fp) in
    Nx (Tree.MOVE (var, unEx init))

(* TODO: Revisit definition, because those stms are kind of fishy *)
let funDec body level =
    let () = procEntryExit level body in
    Nx (Tree.MOVE ((Tree.TEMP Frame.rv), unEx body))
