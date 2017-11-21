%{
  open CallGraphParser
  open CallGraph
%}

%token <string> ID
%token Colon
%token OpenPren ClosePren OpenAngle CloseAngle
%token EOF

%start relation
%type <CallGraphParser.pType> relation
%%

relation: invocation invocation { Call { src = $1; dst = $2 } }
        | javaClass {Class $1 }
        | EOF { Bottom }

invocation: ID Colon ID Colon ID {{ javaClass = $3; javaMethod = $5; typ = parseType $1 }}
          | OpenPren ID ClosePren ID Colon ID {{ javaClass = $4; javaMethod = $6; typ = parseType $2 }}

javaClass : ID Colon ID ID {{srcClass = $3; refClass = $4}}

