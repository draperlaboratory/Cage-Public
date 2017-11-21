(* Lexer for JBC output from javap, as related to my efforts to visualize a class heirarchy.
   @author Jordan Thayer
*)

{
   open Parser
}

rule token = parse
    | [ ' ' '\t' '\n' '\r' ';' ]                                { token lexbuf }
    | ['(']                                             { OpenPren }
    | [')']                                             { ClosePren }
    | '{'                                                   { OpenBracket }
    | '}'                                                   { CloseBracket }
    | ':'                                                   { Colon }
    | "//"[^'\r''\n']*                                          { Comment }
    | "public"                                              { Public "public" }
    | "private"                                             { Private "private" }
    | "protected"                                           { Protected "protected" }
    | "class"                                               { Class }
    | "interface"                                           { Interface }
    | "extends"                                             { Extends }
    | "static"                                              { Static "static" }
    | "implements"                                          { Implements }
    | eof                                                   { EOF }
    | [^'\r' '\n' ' ' '\t']+  as str {ID str}
