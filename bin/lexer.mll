
{

open Parser

let lineNum = Errormsg.lineNum
let linePos = Errormsg.linePos

let pos = Lexing.lexeme_start

let rec crt str = try
  if String.starts_with ~prefix:"\\^" str
  then
    String.make 1 ((((String.sub str 2 1).[0] |> Char.code) - 64 |> char_of_int)) ^
    crt (String.sub str 3 (String.length str - 3))
  else
    String.sub str 0 1 ^ crt (String.sub str 1 (String.length str - 1))
with Invalid_argument _ -> ""

  

let escape str =
  Str.global_replace (Str.regexp "\\\\[\n\r\t ]*\\\\") "" str |>
  (Str.global_replace (Str.regexp "\\\\\\^\\?") "\127") |>
  crt |>
  Scanf.unescaped
}

let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let digits = ['0'-'9']+

let prt = ['!' ' ' '#'-'[' ']'-'~']
let crt = "\\^"['?'-'_']
let esc = '\\'['n' 't' '"' '\\']
let mnln = '\\'['\n' '\t' '\r' ' ']*'\\'
let dig = '\\'['0'-'9']['0'-'9']['0'-'9']

let str = (crt|esc|mnln|dig|prt)+
(*let str = prt+*)

rule token = parse
    | '\n'                 { lineNum := !lineNum+1; linePos := (pos lexbuf) :: !linePos; token lexbuf}

    | "/*"                 { comment lexbuf }

    | "\"\""               { STRING ("", (pos lexbuf)) }
    | '"'(str)'"' as lit   { STRING ((String.sub lit 1 (String.length lit - 2)), (pos lexbuf)) }

    | digits as digits     { INT ((int_of_string digits)) }

    | ":="                 { ASSIGN (pos lexbuf) }
    | '|'                  { OR (pos lexbuf) }
    | '&'                  { AND (pos lexbuf) }
    | ">="                 { GE (pos lexbuf) }
    | '>'                  { GT (pos lexbuf) }
    | "<="                 { LE (pos lexbuf) } 
    | '<'                  { LT (pos lexbuf) }
    | "<>"                 { NEQ (pos lexbuf) }
    | '='                  { EQ (pos lexbuf) }
    | '/'                  { DIVIDE (pos lexbuf) }
    | '*'                  { TIMES (pos lexbuf) }
    | '-'                  { MINUS (pos lexbuf) }
    | '+'                  { PLUS (pos lexbuf) }
    | '.'                  { DOT (pos lexbuf) }
    | '}'                  { RBRACE (pos lexbuf) }
    | '{'                  { LBRACE (pos lexbuf) }
    | ']'                  { RBRACKET (pos lexbuf) }
    | '['                  { LBRACKET (pos lexbuf) }
    | ')'                  { RPAREN (pos lexbuf) }
    | '('                  { LPAREN (pos lexbuf) }
    | ';'                  { SEMICOLON (pos lexbuf) }
    | ':'                  { COLON (pos lexbuf) }
    | ','                  { COMMA (pos lexbuf) }

    | "type"               { TYPE (pos lexbuf) }
    | "var"                { VAR (pos lexbuf) }
    | "function"           { FUNCTION (pos lexbuf) }
    | "break"              { BREAK (pos lexbuf) }
    | "of"                 { OF (pos lexbuf) }
    | "end"                { END (pos lexbuf) }
    | "in"                 { IN (pos lexbuf) }
    | "nil"                { NIL (pos lexbuf) }
    | "let"                { LET (pos lexbuf) }
    | "do"                 { DO (pos lexbuf) }
    | "to"                 { TO (pos lexbuf) }
    | "for"                { FOR (pos lexbuf) }
    | "while"              { WHILE (pos lexbuf) }
    | "then"               { THEN (pos lexbuf) }
    | "else"               { ELSE (pos lexbuf) }
    | "if"                 { IF (pos lexbuf) }
    | "array"              { ARRAY (pos lexbuf) }
    | ident as id          { ID (id, pos lexbuf) }

    | [' ' '\t']           { token lexbuf }    (* Skip whitespace *)
    | eof                  { EOF }
    | _                    { token lexbuf }    (* Skip anything else *)

and comment = parse
    | '\n'        { lineNum := !lineNum+1; linePos := (pos lexbuf) :: !linePos; comment lexbuf}
    | [^'*''/']+  { comment lexbuf }
    | ('*'+)'/'   { token lexbuf }
    | ['*''/']    { comment lexbuf }
