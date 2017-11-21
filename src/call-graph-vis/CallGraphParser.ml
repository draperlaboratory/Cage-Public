(*
  types for parsing output from java-callgraph tool
  @author Jordan Thayer
*)
type tokens =
| OpenPren
| ClosePren
| InvokeVirtual
| InvokeInterface
| InvokeSpecial
| InvokeStatic
| Colon
| ID of string
| EOF

type pType =
| Class of CallGraph.classRelation
| Call of CallGraph.edge
| Bottom


let parseType = function
  | "M" -> CallGraph.Virtual
  | "I" -> CallGraph.Interface
  | "O" -> CallGraph.Special
  | "S" -> CallGraph.Static
  | "C" -> failwith "Called parse type on a class?"
  | _ -> CallGraph.Unknown
