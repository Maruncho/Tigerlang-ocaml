%{
    open struct module A = Absyn end
    open struct module S = Symbol end

    type varType = 
        | Field of Symbol.symbol * int
        | Subscript of A.expr * int

    (*let to_simpleVar sym =*)
    (*    A.*)

    let handle_lvalue init_var l_rest = List.fold_left
        (fun acc el -> begin match el with
            Field (sym, pos) -> A.FieldVar(acc, sym, pos)
          | Subscript (expr, pos) -> A.SubscriptVar(acc, expr, pos)
         end)
        init_var
        l_rest

    let handle_op_expr left oper right pos = A.OpExp
    {
        left = left;
        oper = oper;
        right = right;
        pos = pos
    }

    let handle_unary = handle_op_expr (A.IntExp 0) A.MinusOp
%}

%token <string * int> STRING
%token <int> INT
%token <string * int> ID

%token <int> ASSIGN DOT SEMICOLON COLON COMMA
%token <int> AND OR
%token <int> GE GT LE LT NEQ EQ
%token <int> DIVIDE TIMES MINUS PLUS
%token <int> RBRACE LBRACE RBRACKET LBRACKET RPAREN LPAREN


%token <int> TYPE VAR FUNCTION BREAK OF END IN NIL LET DO TO FOR WHILE THEN ELSE IF ARRAY
%token EOF

%nonassoc LOWEST
%nonassoc DO THEN OF ASSIGN FUNCTION TYPE %right ELSE
%left OR
%left AND
%nonassoc GE GT LE LT NEQ EQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UNARY

%start main
%type <A.expr> main
%%

main:
    expr EOF { $1 }
;

expr:
  | lvalue             { A.VarExp $1 }
  | NIL                { A.NilExp }
  | LPAREN RPAREN      { A.SeqExp ([], $1) } (* no value (unit) *)
  | LPAREN expr RPAREN { A.SeqExp ([$2], $1) } (* grouping *)
  | LPAREN seq RPAREN  { A.SeqExp ($2, $1) }
  | INT                { A.IntExp $1 }
  | STRING             { A.StringExp ((fst $1), (snd $1))} (*had an error for some reason*)
  | arith              { $1 }
  | compare            { $1 }
  | bool               { $1 }
  | new_record         { $1 }
  | new_array          { $1 }
  | conditional        { $1 }
  | valueless          { $1 }
  | call               { $1 }
  | let_exp            { $1 }
;

lvalue:
    ID lvalue_rest { handle_lvalue (A.SimpleVar((S.symbol (fst $1)), (snd $1))) $2 }
;
lvalue_rest:
    DOT ID lvalue_rest                 { (Field ((S.symbol (fst $2)), (snd $2))) :: $3 }
  | LBRACKET expr RBRACKET lvalue_rest { (Subscript ($2, $1)) :: $4 }
  |                                    { [] }
;

seq:
  | expr SEMICOLON expseq { $1 :: $3 }
;
expseq:
    expr                   { [$1] }
  | expr SEMICOLON expseq  { [$1] @ $3 }
;

arith:
    expr PLUS expr          { handle_op_expr $1 A.PlusOp   $3 $2 }
  | expr MINUS expr         { handle_op_expr $1 A.MinusOp  $3 $2 }
  | expr TIMES expr         { handle_op_expr $1 A.TimesOp  $3 $2 }
  | expr DIVIDE expr        { handle_op_expr $1 A.DivideOp $3 $2 }
  | MINUS expr %prec UNARY  { handle_unary $2 $1 }
;
compare:
    expr EQ expr  {handle_op_expr $1 A.EqOp  $3 $2}
  | expr NEQ expr {handle_op_expr $1 A.NeqOp $3 $2}
  | expr GT expr  {handle_op_expr $1 A.GtOp  $3 $2}
  | expr LT expr  {handle_op_expr $1 A.LtOp  $3 $2}
  | expr GE expr  {handle_op_expr $1 A.GeOp  $3 $2}
  | expr LE expr  {handle_op_expr $1 A.LeOp  $3 $2}
;
bool:
    expr AND expr {A.IfExp {test = $1; then' = $3; else' = Some (A.IntExp 0); pos = $2}}
  | expr OR expr  {A.IfExp {test = $1; then' = A.IntExp 1; else' = Some $3; pos = $2}}
;

new_record:
    ID LBRACE separated_list(COMMA, rec_field) RBRACE {A.RecordExp{fields=$3;typ=(S.symbol (fst $1));pos=$2}}
;
rec_field:
    ID EQ expr { ((S.symbol (fst $1)), $3, $2) }
;

new_array:
    ID LBRACKET expr RBRACKET OF expr {A.ArrayExp {typ = (S.symbol (fst $1)); size = $3; init = $6; pos = $2}}
;

conditional:
    IF expr THEN expr                  {A.IfExp {test = $2; then' = $4; else' = None; pos = $1}}
  | IF expr THEN expr ELSE expr        {A.IfExp {test = $2; then' = $4; else' = Some $6; pos = $1}}
;

valueless:
  | lvalue ASSIGN expr                 {A.AssignExp {var = $1; expr = $3; pos = $2}}
  | WHILE expr DO expr                 {A.WhileExp {test = $2; body = $4; pos = $1}}
  | FOR ID ASSIGN expr TO expr DO expr {A.ForExp{var=(S.symbol (fst $2));lo=$4;hi=$6;body=$8;pos=$1;escape=ref true}}
  | BREAK                              {A.BreakExp $1}
;

call:
    ID LPAREN separated_list(COMMA, expr) RPAREN {A.CallExp {func = (S.symbol (fst $1)); args = $3; pos = $2}}
;

let_exp:
    LET decs IN separated_list(SEMICOLON, expr) END
            {A.LetExp {decs = $2; body = A.SeqExp($4, $3); pos = $1}}
;

decs:
    dec decs { $1 :: $2}
  |          { [] }
;

dec:
    tydecs %prec LOWEST  { A.TypeDec $1 }
  | vardec                                  { $1 }
  | fundecs %prec LOWEST { A.FunctionDec $1 }
;

tydecs:
    tydec { [$1] }
  | tydecs tydec { $1 @ [$2] }
;
tydec:
    TYPE ID EQ ty { let r : A.typeDecRecord = {name = (S.symbol (fst $2)); ty = $4; pos = $1} in r }
;

ty:
    ID                      { A.NameTy((S.symbol (fst $1)), (snd $1)) }
  | LBRACE tyfields RBRACE  { A.RecordTy $2 }
  | ARRAY OF ID             { A.ArrayTy((S.symbol (fst $3)), $1) }
;

tyfields:
    tyfields_seq { $1 }
  |              { [] }
;
tyfields_seq:
    ID COLON ID COMMA tyfields_seq
            { let r : A.field = {name = (S.symbol (fst $1)); typ = (S.symbol (fst $3)); pos = $2; escape = ref true} in [r] @ $5 }
  | ID COLON ID
            { let r : A.field = {name = (S.symbol (fst $1)); typ = (S.symbol (fst $3)); pos = $2; escape = ref true} in [r] }
;

vardec:
    VAR ID ASSIGN expr          {A.VarDec{name=(S.symbol (fst $2));typ=None;init=$4;pos=$1;escape=ref true}}
  | VAR ID COLON ID ASSIGN expr {A.VarDec{name=(S.symbol (fst $2));typ=Some((S.symbol (fst $4)),$3);init=$6;pos=$1;escape=ref true}}
;

fundecs: fundec { [$1] }
       | fundecs fundec { $1 @ [$2] }
;
fundec:
    FUNCTION ID LPAREN tyfields RPAREN EQ expr {let r : A.fundec =
        {name = (S.symbol (fst $2)); params = $4; result = None; body = $7; static_link = ref false; pos = $1} in r}
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ expr {let r : A.fundec =
        {name = (S.symbol (fst $2)); params = $4; result = Some ((S.symbol (fst $7)), $6); body = $9; static_link = ref false; pos = $1} in r}
;
