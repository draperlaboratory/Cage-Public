
type tokens =
| OpenPren
| ClosePren
| OpenBracket
| CloseBracket
| ID of string
| Colon
| EOF
| Comment
| Public of string
| Private of string
| Protected of string
| Class
| Interface
| Extends
| Implements
| Static

type javaClass = {
  name : string;
  modifiers : string list;
  parent : string option;
  containedIn : javaClass option;
  isClass : bool;
}

type parseReturn =
| Class of javaClass
| Skip
| EndOfFile

let (classStack : javaClass list ref) = ref []
let push c = classStack := c::!classStack
let pop () =
  match !classStack with
    | [] -> failwith "popping empty stack!"
    | _::tl -> begin
      classStack := tl;
      Skip
    end

let makeClass ?(isClass = true) name (modifiers : string list) parent =
  let c = { name;
            modifiers;
            parent;
            isClass;
            containedIn = match !classStack with
            | [] -> None
            | hd::_ -> Some hd;} in
  push c;
  c
