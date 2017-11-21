%{
  open Jbc_parser
%}

%token Comment
%token Colon
%token OpenPren OpenBracket ClosePren CloseBracket
%token <string> Public Private Protected Static
%token Class Interface
%token Extends Implements
%token <string> ID
%token EOF

%start jbc
%type <Jbc_parser.parseReturn> jbc
%%

jbc: javaClass  { Class $1 }
   | classClose { $1 }
   | vaccuous   { $1 }
   | EOF { EndOfFile }

javaClass: Public Static Class ID Extends ID OpenBracket                  { makeClass $4 [$1; $2;] (Some $6) }
         | Private Static Class ID Extends ID OpenBracket                 { makeClass $4 [$1; $2;] (Some $6) }
         | Protected Static Class ID Extends ID OpenBracket               { makeClass $4 [$1; $2;] (Some $6) }
         | Public Static Class ID OpenBracket                             { makeClass $4 [$1; $2] None }
         | Private Static Class ID OpenBracket                            { makeClass $4 [$1; $2] None }
         | Protected Static Class ID OpenBracket                          { makeClass $4 [$1; $2] None }
         | Public Class ID Extends ID OpenBracket                         { makeClass $3 [$1] (Some $5) }
         | Private Class ID Extends ID OpenBracket                        { makeClass $3 [$1] (Some $5) }
         | Protected Class ID Extends ID OpenBracket                      { makeClass $3 [$1] (Some $5) }
         | Public Class ID OpenBracket                                    { makeClass $3 [$1] None }
         | Private Class ID OpenBracket                                   { makeClass $3 [$1] None }
         | Protected Class ID OpenBracket                                 { makeClass $3 [$1] None }
         | Class ID OpenBracket                                           { makeClass $2 [] None }
         | Static Class ID OpenBracket                                    { makeClass $3 [$1] None }
         | Public Static Class ID Extends ID OpenBracket Implements ID    { makeClass $4 [$1; $2;] (Some $6) }
         | Private Static Class ID Extends ID OpenBracket Implements ID   { makeClass $4 [$1; $2;] (Some $6) }
         | Protected Static Class ID Extends ID OpenBracket Implements ID { makeClass $4 [$1; $2;] (Some $6) }
         | Public Static Class ID OpenBracket Implements ID               { makeClass $4 [$1; $2] None }
         | Private Static Class ID OpenBracket Implements ID              { makeClass $4 [$1; $2] None }
         | Protected Static Class ID OpenBracket Implements ID            { makeClass $4 [$1; $2] None }
         | Public Class ID Extends ID OpenBracket Implements ID           { makeClass $3 [$1] (Some $5) }
         | Private Class ID Extends ID OpenBracket Implements ID          { makeClass $3 [$1] (Some $5) }
         | Protected Class ID Extends ID OpenBracket Implements ID        { makeClass $3 [$1] (Some $5) }
         | Public Class ID OpenBracket Implements ID                      { makeClass $3 [$1] None }
         | Private Class ID OpenBracket Implements ID                     { makeClass $3 [$1] None }
         | Protected Class ID OpenBracket Implements ID                   { makeClass $3 [$1] None }
         | Class ID OpenBracket Implements ID                             { makeClass $2 [] None }
         | Static Class ID OpenBracket Implements ID                      { makeClass $3 [$1] None }

jIface: Public Interface ID { makeClass ~isClass:false $3 [$1] None}
      | Private Interface ID { makeClass ~isClass:false $3 [$1] None}
      | Protected Interface ID { makeClass ~isClass:false $3 [$1] None}
      | Interface ID { makeClass ~isClass:false $2 [] None}
      | Public Interface ID Extends ID { makeClass ~isClass:false $3 [$1] (Some $5)}
      | Private Interface ID Extends ID { makeClass ~isClass:false $3 [$1] (Some $5)}
      | Protected Interface ID Extends ID { makeClass ~isClass:false $3 [$1] (Some $5)}
      | Interface ID Extends ID { makeClass ~isClass:false $2 [] (Some $4)}

classClose: CloseBracket { pop() }

vaccuous: Comment             { Skip }
        | Colon               { Skip }
        | OpenPren            { Skip }
        | ClosePren           { Skip }
        | Public ID           { Skip }
        | Private ID          { Skip }
        | Protected ID        { Skip }
        | Static ID           { Skip }
        | Public Static ID    { Skip }
        | Private Static ID   { Skip }
        | Protected Static ID { Skip }
        | ID                  { Skip }

