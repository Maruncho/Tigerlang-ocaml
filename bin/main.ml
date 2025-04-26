(*let rec crt str = try*)
(*  if String.starts_with ~prefix:"\\^" str*)
(*  then*)
(*    String.make 1 ((((String.sub str 2 1).[0] |> Char.code) - 64 |> char_of_int)) ^*)
(*    crt (String.sub str 3 (String.length str - 3))*)
(*  else*)
(*    String.sub str 0 1 ^ crt (String.sub str 1 (String.length str - 1))*)
(*with Invalid_argument _ -> ""*)
(**)
(**)
(**)
(*let escape str =*)
(*  Str.global_replace (Str.regexp "\\\\[\n\r\t ]*\\\\") "" str |>*)
(*  (Str.global_replace (Str.regexp "\\\\\\^\\?") "\127") |>*)
(*  crt |>*)
(*  Scanf.unescaped*)
(**)
(*let _ = print_string (escape "Hell\076o \\n \\t \\\" \\^? World\\ \n \n \t \t \r \r \\!")*)

let _ = try
  let file = open_in "tests/test42.tig" in
  let lexbuf = Lexing.from_channel file in
  let result = Parser.main Lexer.token lexbuf in
  let {exp;_}:Semant.expty = Semant.transProg result in
  PrintTree.printTree stdout (Translate.unNx exp)
with
  Parser.Error -> print_string "There is an syntactical error. Too lazy to learn menhir error-handling.\n"
