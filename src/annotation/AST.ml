(* representation of the polynomials we're reading in *)

type algebraic =
| Constant of int
| Variable of string
| Multiplication of (algebraic * algebraic)
| Addition of (algebraic * algebraic)
| Log of algebraic
| Negated of algebraic
| Pow of pow
and pow = {
  base : algebraic;
  exp : algebraic;
}

let rec polyToString = function
  | Constant i -> Printf.sprintf "%i" i
  | Variable s -> s
  | Multiplication (p1, p2) -> "(" ^ (polyToString p1) ^ " * " ^ (polyToString p2) ^ ")"
  | Addition (p1, p2) -> "(" ^ (polyToString p1) ^ " + " ^ (polyToString p2) ^ ")"
  | Negated p1 -> "-" ^ (polyToString p1)
  | Pow {base=base; exp=exp} -> (polyToString base) ^ "^" ^ "(" ^ (polyToString exp) ^ ")"
  | Log p -> "log " ^ "(" ^ (polyToString p) ^ "^)"
