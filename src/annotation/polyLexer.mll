{
  open PolyParser
  open AST
}

rule token = parse
    [' ' '\t' '\n']                     { token lexbuf } (* skip whitespace *)
  | ['0'-'9']+ as lxm                   { INT (int_of_string lxm) }
  | "log"                               { LOG }
  | ['a'-'z' 'A'-'Z']+['0'-'9']* as lxm { VARIABLE lxm }
  | '+'                                 { PLUS }
  | '-'                                 { MINUS }
  | '*'                                 { TIMES }
  | '('                                 { OPENPAREN }
  | ')'                                 { CLOSEPAREN }
  | '^'                                 { POW }
  | eof                                 { EOF }
