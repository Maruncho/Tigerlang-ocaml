  let anyErrors = ref false
  let fileName = ref ""
  let lineNum = ref 1
  let linePos = ref [1]
  let sourceStream = ref Stdlib.stdin

  let reset () = (anyErrors:=false;
		 fileName := "";
		 lineNum := 1;
		 linePos := [1];
		 sourceStream := Stdlib.stdin)

  exception Error

  let error pos (msg:string) =
    let rec look lst n = (match lst with
	| a :: rest ->
	  if a < pos then print_string (":" ^
			 string_of_int n ^
			 "." ^
			 string_of_int (pos-a))
		 else look rest (n-1) 
	| _ -> print_string "0.0")
    in anyErrors := true;
	print_string (!fileName);
	look !linePos !lineNum;
	print_string ":";
	print_string msg;
	print_string "\n"

let impossible msg =
    print_string ("Error: Compiler bug: " ^ msg ^ "\n");
    Stdlib.flush stdout;
    raise Error
