%{
open AST
%}

%token <int> INT
%token <string> VARIABLE
%token PLUS MINUS TIMES
%token LOG
%token POW
%token OPENPAREN CLOSEPAREN
%token EOF
%left PLUS
%left EOF
%nonassoc POW
%left TIMES
%nonassoc MINUS
%start main
%type <algebraic> main
%%

main:
    expr EOF { $1 }
;
expr:
 | stmt                          { $1 }
 | stmt stmt                     { Multiplication ($1, $2)}
 | OPENPAREN expr CLOSEPAREN     { $2 }
 | expr TIMES expr               { Multiplication ($1, $3) }
 | expr PLUS expr                { Addition ($1, $3)}
 | MINUS expr                    { Negated $2}
 | stmt MINUS stmt               { Addition ($1, Negated $3) }
 | LOG OPENPAREN expr CLOSEPAREN { Log $3}
 | expr POW expr                 { Pow {base = $1; exp = $3;}}


stmt:
   INT                           { Constant $1 }
 | VARIABLE                      { Variable $1 }
