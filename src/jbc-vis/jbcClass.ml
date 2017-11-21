type modifier =
| Private
| Protected
| Public
| Static

type javaClass = {
  name : string;
  modifiers : modifier list;
  parent : javaClass option;
  contains : javaClass list; (* Could also be a string list *)
}

let compareClasses jc1 jc2 =
  compare jc1.name jc2.name

module ClassMap = Map.Make(String)

let getModifier = function
  | "private" -> Private
  | "public" -> Public
  | "protected" -> Protected
  | "static" -> Static
  | _ -> failwith "Unrecognized modifier"


let replace cm el =
  ClassMap.add el.name el (ClassMap.remove el.name cm)


let handleParsedClasses (prototypes : Jbc_parser.javaClass list) =
  let convertPrototype p =
    {
      name = p.Jbc_parser.name;
      modifiers = List.map getModifier p.Jbc_parser.modifiers;
      parent = None;
      contains = [];
    } in
  let initialPass = List.fold_left (fun accum p ->
    let asClass = convertPrototype p in
    ClassMap.add asClass.name asClass accum) ClassMap.empty prototypes in
  (* no parents, no contained in *)
  let withParents = List.fold_left (fun accum p ->
    match p.Jbc_parser.parent with
    | None -> accum
    | Some pName -> begin
      let parentClass = ClassMap.find pName accum in
      let thisClass = ClassMap.find p.Jbc_parser.name accum in
      replace accum { thisClass with parent = Some parentClass }
    end)
    initialPass prototypes in
  (* parents are fixed *)
  let withContainedClasses =
    List.fold_left (fun accum p ->
      match p.Jbc_parser.containedIn with
      | None -> accum
      | Some jbcp -> begin
        let contained = ClassMap.find p.Jbc_parser.name accum in
        let container = ClassMap.find jbcp.Jbc_parser.name accum in
        replace accum
          { container with contains = contained::container.contains }
      end
    ) withParents prototypes
  in
  (* contains are fixed *)
  withContainedClasses

module ClassHash = struct
  type t = javaClass
  let compare = compareClasses
  let equal a b = 0 = compare a b
  let hash cls = Hashtbl.hash cls.name
end

module ClassGraph = Graph.Imperative.Digraph.ConcreteBidirectional(ClassHash)

let drawClassGraph map g fname =
  let module Vis = Graph.Graphviz.Dot(struct
    include ClassGraph
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [`Shape `Box; `Label v.name]
    let vertex_name v = Printf.sprintf "\"%s\"" v.name
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end) in
  let fdesc = Unix.openfile fname [Unix.O_CREAT; Unix.O_WRONLY;] 0o640 in
  let fStream = Unix.out_channel_of_descr fdesc in
  Vis.output_graph fStream g;
  Unix.close fdesc

let buildClassGraph map =
  let g = ClassGraph.create () in
  let addRelation _ jc =
    ClassGraph.add_vertex g jc;
    match jc.parent with
    | None -> ()
    | Some p ->
      begin
        ClassGraph.add_vertex g p;
        ClassGraph.add_edge g jc p
      end in
  ClassMap.iter addRelation map;
  g

let visClassGraph crs fname =
  let g = buildClassGraph crs in
  drawClassGraph crs g fname
