type invType = Virtual | Interface | Special | Static | Unknown
type call = { javaClass : string; javaMethod : string; typ : invType; }
type classRelation = { srcClass : string; refClass : string; }
type edge = { src : call; dst : call; }
val visCallGraph : edge list -> string -> unit
val visClassGraph : classRelation list -> string -> unit
