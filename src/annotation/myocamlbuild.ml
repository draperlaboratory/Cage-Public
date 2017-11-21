open Ocamlbuild_plugin

(* Hook handler. This is where our extensions for the [ocamlbuild] system lie.
 *
 * It is registered automatically when [ocamlbuild] is called. *)
let hook_handler = function
     After_rules ->
        (* Overrides ocamlyacc-generated parser.mli with parser.override.mli *)
        copy_rule "Overrides Parser interface" ~insert: (`before "ocamlyacc")
           "polyParser.override.mli" "polyParser.mli"
  | _ -> ()

(* Registers our hook handler when this module is loaded (by ocamlbuild). *)
let _ = dispatch hook_handler
