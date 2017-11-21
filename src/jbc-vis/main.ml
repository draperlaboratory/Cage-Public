let (inputJar : string option ref) = ref None
let (inputJBC : string option ref) = ref None
let (outputDirectory : string option ref) = ref None
let speclist = [("--out-dir", Arg.String (fun s -> outputDirectory := Some s),
                 "- Set the output location for the visualizations.");
                ("--jar", Arg.String (fun s -> inputJar := Some s),
                 "- Set the location of the jar to be visualized");]
and usage = Printf.sprintf "usage: %s <jbc-file-path> --jar <jar-file-path> --out-dir <path>" Sys.argv.(0)

let print_usage () = Printf.printf "%s\n" usage

(** Convert input path and output directory to a pair of dot file paths.
    These tell us where to store the dot files describing class->class and
    method -> method relationships. *)
let makeOutName jbcPath outdir =
  let base =  Filename.basename jbcPath in
  let noext = try Filename.chop_extension base with _ -> base in
  let cString = Printf.sprintf "%s/%s-class.dot" outdir noext in
  cString

(** Convert dot files to pdfs for human consumption *)
let visDot dotFile =
  let pdfName = (Filename.chop_extension dotFile) ^ ".pdf" in
  let retVal = Unix.system (Printf.sprintf "dot -Tpdf %s -o %s" dotFile pdfName) in
  if retVal <> Unix.WEXITED 0
  then Printf.eprintf "dot didn't succesfully visualize %s as a pdf.\n%!" dotFile

let getMap ipath =
  let ichan = open_in ipath in
  let lexbuf = Lexing.from_channel ichan in
  let rec parse () =
    match Parser.jbc Lex.token lexbuf with
    | Jbc_parser.Class c -> c :: parse ()
    | Jbc_parser.Skip -> parse ()
    | Jbc_parser.EndOfFile -> [] in
  let ret = parse () in
  close_in ichan;
  JbcClass.handleParsedClasses ret


let run ipath outdir =
  let classMap = getMap ipath in
  let outName = makeOutName ipath outdir in
  JbcClass.visClassGraph classMap outName;
  visDot outName


let main () =
  Arg.parse speclist (fun s -> inputJBC := Some s) usage;
  match !inputJBC with
  | Some s -> begin
    match !outputDirectory with
    | None -> ( print_usage(); failwith "No output directory specified." )
    | Some o ->
      run s o
  end
  | None -> begin
    print_usage();
    failwith "No Arguments were supplied."
  end

let _ = main ()
