open AST

let tests =
  ["1";
   "2 * x + 3";
   "x";
   "x1";
   "x + 1";
   "2 * x";
   "x * 2";
   "x^2";
   "3 * x^2 + 17";
   "-3x";
   "-3 * x";
   "x - 3"; (* this seems wrong *)
   "x^-3";
   "3 * (x^2 + 17)";
   "3x";
   "3x1";
   "x * y + z";
   "z + x * y";
   "a + b * c + d * e + f";
   "y * (x^-3)";
   "y * (x^(4 + y - 2))";
   "y * (x ^ 2 + -3)";
  ]

let run (s : string) =
  Printf.printf "Parsing %s\n" s;
  let lexbuf = Lexing.from_string s in
  let result = PolyParser.main PolyLexer.token lexbuf in
  Printf.printf "%s\n" (polyToString result)

let _ =
  List.iter run tests
