open Yojson.Basic
open Yojson.Basic.Util
open PolyParser
open AST

(********** Example input ***************)


type jtype =
| Int
| Object

type argument = {
  name : string;
  jtype : jtype;
}

type argList = argument list

type complexity =
| Unknown
| Constant
| Expression of algebraic

type complexitySpec = {
  upperTime : complexity;
  lowerTime : complexity;
  upperMemory : complexity;
  lowerMemory : complexity;
}

type functionSpec = {
  fname : string;
  args : argList;
  secretArgs : int list; (* argument positions *)
  complexity : complexitySpec;
  pure : bool;
}

type package = {
  pname : string;
  created : string;
  functions : functionSpec list;
}

let parseTime (timeJson : json) =
  match timeJson with
    `String s -> s
  | _ -> failwith "Excepted the time to be a json string, but it wasn't!"

let fetchAssoc (recordFields : (string * json) list) (key : string) =
  try
    List.assoc key recordFields
  with Not_found -> failwith (Printf.sprintf "Couldn't find %s" key)

let parseComplexityString (s : string) =
  let s' = String.uppercase s in
  match s' with
  | "UNKNOWN" -> Unknown
  | "CONSTANT" -> Constant
  | _ ->
    let lexbuf = Lexing.from_string s in
    let result = PolyParser.main PolyLexer.token lexbuf in
    Expression result

let parseComplexity (complexityRep : json) =
  match complexityRep with
  | `Assoc recordFields ->
    let get = fetchAssoc recordFields in
    let upCompString = to_string (get "upperTime")
    and lowCompString = to_string (get "lowerTime")
    and upMemString = to_string (get "upperMemory")
    and lowMemString = to_string (get "lowerMemory") in
    { upperTime = parseComplexityString upCompString;
      lowerTime = parseComplexityString lowCompString;
      upperMemory = parseComplexityString upMemString;
      lowerMemory = parseComplexityString lowMemString; }
  | _ -> failwith "Excepted Complexity Representation to be an assoc!"

let parseType (typeRep : json) =
  Int (* stub *)

let parseArg (argRep : json) =
  match argRep with
  | `Assoc recordFields ->
    let get = fetchAssoc recordFields in
    let name = to_string (get "varName")
    and typeJson = get "varType" in
    { name = name;
      jtype = parseType typeJson; }
  | _ -> failwith "Expected Argument Representation to be an assoc!"

let parseArgList (argListRep : json) =
  List.map parseArg (to_list argListRep)

let parseSecretList (secretListRep : json) =
  List.map to_int (to_list secretListRep)

let parseFunction (functionRep : json) =
  match functionRep with
  | `Assoc recordFields ->
    let get = fetchAssoc recordFields in
    let nameJson = get "name"
    and argsJson = get "args"
    and secretJson = get "secretArgs"
    and complexityJson = get "complexity"
    and purityJson = get "pure" in
    { fname = to_string nameJson;
      args = parseArgList argsJson;
      secretArgs = parseSecretList secretJson;
      complexity = parseComplexity complexityJson;
      pure = to_bool purityJson }
  | _ -> failwith "Excepted Function Representation to be an assoc!"

let parseFunctionList (functionsJson : json) =
  match functionsJson with
  | `List jsonObjs -> List.map parseFunction jsonObjs
  | _ -> failwith "Expected functions list to be of type list."

let parseRecord (jsonObj : json) =
  match jsonObj with
  | `Assoc recordFields ->
    begin
      let get = fetchAssoc recordFields in
      let timeJson = get "created"
      and packageName = get "package"
      and functionList = get "functions" in
      let time = parseTime timeJson
        and functionList = parseFunctionList functionList in
        { pname = to_string packageName;
          created  = time;
          functions = functionList; }
    end
    | _ -> failwith "Tried to parse record on not an assoc!"

