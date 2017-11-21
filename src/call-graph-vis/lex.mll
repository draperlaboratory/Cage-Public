(*
  Lexer for the java-callgraph output
  @author Jordan Thayer
*)
{
  open Parser
}

rule token = parse
    | [' ' '\t' '\n' '\r' ]                     { token lexbuf }
    | '('                                       { OpenPren }
    | ')'                                       { ClosePren }
    | ['A'-'Z' 'a'-'z' '0'-'9' '_' '.' '<' '>' '[']* as str { ID str }
    | ':'                                       { Colon }
    | eof                   { EOF }
    | _ as c { failwith (Printf.sprintf "unrecognized character: %c" c)}
