
type  exp = CONST of int
          | MAXINT (*ocaml's int is cringe*)
          | NAME of Temp.label
          | TEMP of Temp.temp
          (*| TEMP_PTR of Temp.temp*)
          | BINOP of binop * exp * exp
          | MEM of exp
          (*| MEM_FAT of exp * exp*)
          | CALL of exp * exp list
          | ESEQ of stm * exp

          | RELOP of relop * exp * exp (*like binop, but for comparison*)
          | NEG of exp
          | UNDEFINED

and  stm  = MOVE of exp * exp
          | EXP of exp
          | JUMP of exp * Temp.label list
          | CJUMP of exp * Temp.label * Temp.label
          | SEQ of stm * stm
          | LABEL of Temp.label

          (*| CMOVE of exp * exp*)

and binop = PLUS | MINUS | MUL | DIV
          | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

and relop = EQ | NE | LT | GT | LE | GE
          | ULT | ULE | UGT | UGE
