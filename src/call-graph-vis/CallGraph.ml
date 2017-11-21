(*
  Simple representation of java call graph, along with
  some code for visualizing same using ocamlgraph
  @author Jordan Thayer
*)
type invType =
| Virtual
| Interface
| Special
| Static
| Unknown


type call = {
  javaClass : string;
  javaMethod : string;
  typ : invType;
}

(* srcClass invokes methods in refClass *)
type classRelation = {
  srcClass : string;
  refClass : string;
}

(* src function makes call to dst function *)
type edge = {
  src : call;
  dst : call;
}

let compareCall c1 c2 =
  if c1 = c2 then 0
  else
    let classComp = compare c1.javaClass c2.javaClass in
    if classComp <> 0
    then classComp
    else compare c1.javaMethod c2.javaMethod


let compareCR c1 c2 =
  if c1 = c2 then 0
  else
    let srcComp = compare c1.srcClass c2.srcClass in
    if srcComp <> 0
    then srcComp
    else compare c1.refClass c2.refClass

module CallHash = struct
  type t = call
  let compare = compareCall
  let equal a b = 0 = (compareCall a b)
  let hash call = Hashtbl.hash (call.javaClass, call.javaMethod, call.typ)
end

module ClassHash = struct
  type t = String.t
  let compare = compare
  let equal a b = 0 = (compare a b)
  let hash cls = Hashtbl.hash cls
end

module CallGraph = Graph.Imperative.Digraph.ConcreteBidirectional(CallHash)
module ClassGraph = Graph.Imperative.Digraph.ConcreteBidirectional(ClassHash)

let drawClassGraph g fname =
  let module Vis = Graph.Graphviz.Dot(struct
    include ClassGraph
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [`Shape `Box; `Label v]
    let vertex_name v = Printf.sprintf "\"%s\"" v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end) in
  let fdesc = Unix.openfile fname [Unix.O_CREAT; Unix.O_WRONLY;] 0o640 in
  let fStream = Unix.out_channel_of_descr fdesc in
  Vis.output_graph fStream g;
  Unix.close fdesc

let buildClassGraph (crs : classRelation list) =
  let g = ClassGraph.create () in
  let handleCR cr =
    ClassGraph.add_vertex g cr.srcClass;
    ClassGraph.add_vertex g cr.refClass;
    ClassGraph.add_edge g cr.srcClass cr.refClass in
  List.iter handleCR crs;
  g

let drawCallGraph g fname =
  let module Vis = Graph.Graphviz.Dot(struct
    include CallGraph
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v =
      [`Shape `Box;
       `Label (Printf.sprintf "%s#%s" v.javaClass v.javaMethod)]
    let vertex_name v = Printf.sprintf "\"%s#%s\"" v.javaClass v.javaMethod
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end) in
  let fdesc = Unix.openfile fname [Unix.O_CREAT; Unix.O_WRONLY;] 0o640 in
  let fStream = Unix.out_channel_of_descr fdesc in
  Vis.output_graph fStream g;
  Unix.close fdesc

let buildCallGraph (calls : edge list) =
  let g = CallGraph.create () in
  let handleCall c =
    CallGraph.add_vertex g c.src;
    CallGraph.add_vertex g c.dst;
    CallGraph.add_edge g c.src c.dst in
  List.iter handleCall calls;
  g

let visClassGraph (crs : classRelation list) fname =
  let g = buildClassGraph crs in
  drawClassGraph g fname

let visCallGraph (calls : edge list) fname =
  let g = buildCallGraph calls in
  drawCallGraph g fname
