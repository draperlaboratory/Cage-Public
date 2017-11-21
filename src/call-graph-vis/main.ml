(*
  Produce a dot-visualization of the call graph for java jars.
  Relies on external tool java-callgraph, available at https://github.com/gousiosg/java-callgraph
  @author Jordan Thayer
*)

let (inputJar : string option ref) = ref None
let (outputDirectory : string option ref) = ref None
let (jcallgraphPath : string ref) = ref "lib/java-callgraph/target/javacg-0.1-SNAPSHOT-static.jar"

let speclist =
  [ ("--jar", Arg.String (fun s -> inputJar := Some s),
     "- Set the location of the jar to be visualized");
    ("--out-dir", Arg.String (fun s -> outputDirectory := Some s),
     "- Set the output location for the visualizations.");
    ("--jcallgraph", Arg.String (fun s -> jcallgraphPath := s),
     "- Set the location of the java-callgraph jar.");
  ]
and usage = Printf.sprintf "usage: %s --jar <jar-file-path> --out-dir <path>" Sys.argv.(0)

let print_usage () =
  Arg.usage speclist usage


(** Convert input path and output directory to a pair of dot file paths.
    These tell us where to store the dot files describing class->class and
    method -> method relationships. *)
let makeOutNames ipath outdir =
  let base =  Filename.basename ipath in
  let noext = try Filename.chop_extension base with _ -> base in
  let cString = Printf.sprintf "%s/%s-class.dot" outdir noext
  and mString = Printf.sprintf "%s/%s-call.dot" outdir noext in
  cString, mString

(** Parses the output of java-call-graph-static to produce an ocamlgraph
    representation of the method invocations of the java program being studied. *)
let parse inchan  =
  let lexbuf = Lexing.from_channel inchan in
  let rec parse classAccum methodAccum =
    match Parser.relation Lex.token lexbuf with
    | CallGraphParser.Bottom -> classAccum,  methodAccum
    | CallGraphParser.Class cr -> parse (cr::classAccum) methodAccum
    | CallGraphParser.Call mr -> parse classAccum (mr :: methodAccum)
  in
  let classRelations, methodRelations = parse [] [] in
  close_in inchan;
  classRelations, methodRelations

(** Convert dot files to pdfs for human consumption *)
let visDot dotFile =
  let pdfName = (Filename.chop_extension dotFile) ^ ".pdf" in
  let retVal = Unix.system (Printf.sprintf "dot -Tpdf %s -o %s" dotFile pdfName) in
  if retVal <> Unix.WEXITED 0
  then Printf.eprintf "dot didn't succesfully visualize %s as a pdf.\n%!" dotFile

let invokeJavaCallGraph ipath =
  match !inputJar with
  | None -> failwith "No specified input jar!"
  | Some path ->
    let cmdString = Printf.sprintf "java -jar %s %s" !jcallgraphPath path in
    let sout, sin = Unix.open_process cmdString in
    let ret = parse sout in
    let processStatus = Unix.close_process (sout,sin) in
    match processStatus with
    | Unix.WEXITED 0 -> ret
    | _ -> failwith (Printf.sprintf "%s didn't properly execute." cmdString)

let run ipath outDir =
  let classRelations, methodRelations = invokeJavaCallGraph ipath
  and classOut, methodOut = makeOutNames ipath outDir in
  (* generate dot files *)
  CallGraph.visClassGraph classRelations classOut;
  CallGraph.visCallGraph methodRelations methodOut;
  visDot classOut;
  visDot methodOut

let main () =
  Arg.parse speclist (fun _ -> ()) usage;
  match (!inputJar, !outputDirectory) with
  | Some ipath, Some opath -> run ipath opath
  | _, _ -> begin
    print_usage();
    failwith "Need to specify both a jar file and an output directory."
  end

let _ = main()
