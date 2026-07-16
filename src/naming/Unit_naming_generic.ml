open Common
open Fpath_.Operators

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = "tests"

(* The resolutions of every expression-level use of [name], in source
   order. Definition-site names (def/var entities, directives) are not
   expressions and are not collected. *)
let resolutions_of_name ast name =
  let acc = ref [] in
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_expr venv e =
        (match e.AST_generic.e with
        | AST_generic.N (AST_generic.Id ((s, _), id_info)) when s = name ->
            acc := !(id_info.AST_generic.id_resolved) :: !acc
        | _ -> ());
        super#visit_expr venv e
    end
  in
  visitor#visit_program () ast;
  List.rev !acc

let kind_of_resolution = function
  | None -> "Unresolved"
  | Some ((kind : AST_generic.resolved_name_kind), _sid) -> (
      match kind with
      | AST_generic.LocalVar -> "LocalVar"
      | AST_generic.Parameter -> "Parameter"
      | AST_generic.Global -> "Global"
      | AST_generic.ImportedEntity _ -> "ImportedEntity"
      | AST_generic.ImportedModule _ -> "ImportedModule"
      | _ -> "Other")

let check_resolutions ast name expected =
  let actual =
    resolutions_of_name ast name |> List.map kind_of_resolution
  in
  Alcotest.(check (list string))
    (spf "resolutions of '%s'" name)
    expected actual

(* All resolved uses of [name] refer to one and the same binding (sid). *)
let check_single_binding ast name =
  let sids =
    resolutions_of_name ast name
    |> List.filter_map (Option.map (fun (_kind, sid) -> sid))
    |> List.map AST_generic.SId.show
  in
  match sids with
  | [] -> Alcotest.failf "no resolved uses of '%s'" name
  | first :: rest ->
      rest
      |> List.iter (fun sid ->
             Alcotest.(check string)
               (spf "all uses of '%s' share one binding" name)
               first sid)

let tests parse_program =
  Testo.categorize "naming generic"
    [
      t "regression files" (fun () ->
          let dir = Filename.concat tests_path "naming/python" in
          let files1 = Common2.glob (spf "%s/*.py" dir) in
          let dir = Filename.concat tests_path "naming/go" in
          let files2 = Common2.glob (spf "%s/*.go" dir) in
          let dir = Filename.concat tests_path "naming/js" in
          let files3 = Common2.glob (spf "%s/*.js" dir) in
          let dir = Filename.concat tests_path "naming/java" in
          let files4 = Common2.glob (spf "%s/*.java" dir) in
          let dir = Filename.concat tests_path "naming/ruby" in
          let files5 = Common2.glob (spf "%s/*.rb" dir) in
          let dir = Filename.concat tests_path "naming/crystal" in
          let files6 = Common2.glob (spf "%s/*.cr" dir) in
          let dir = Filename.concat tests_path "naming/php" in
          let files7 = Common2.glob (spf "%s/*.php" dir) in

          files1 @ files2 @ files3 @ files4 @ files5 @ files6 @ files7
          |> Fpath_.of_strings
          |> List.iter (fun file ->
                 try
                   (* at least we can assert we don't thrown an exn or go
                      into infinite loops *)
                   let ast = parse_program file in
                   let lang = Lang.lang_of_filename_exn file in
                   Naming_AST.resolve lang ast;
                   (* this used to loop forever if you were not handling correctly
                      possible cycles with id_type *)
                   let _v = AST_generic.show_any (AST_generic.Pr ast) in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %s" !!file));
      t "python local shadows module function" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/python/shadow_global_fn.py")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Python ast;
          (* Assignment makes `query` function-local even though the module
             defines `def query`; both the assignment target and the later
             use must be LocalVar, not Global. *)
          check_resolutions ast "query" [ "LocalVar"; "LocalVar" ];
          (* Under a `global` directive the assignment rebinds the
             module-level variable. Occurrences: module-level `counter = 0`,
             the function-scope assignment, the use. *)
          check_resolutions ast "counter" [ "Global"; "Global"; "Global" ];
          (* Imports stay flow-insensitive (pdb.yaml ecosystem constraint):
             `jsonlib = make()` does not shadow `import json as jsonlib`. *)
          check_resolutions ast "jsonlib"
            [ "ImportedModule"; "ImportedModule" ]);
      t "ruby assignment shadows toplevel def" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/ruby/shadow_global_fn.rb")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Ruby ast;
          (* Assignment makes `query` method-local even though the top
             level defines `def query`. *)
          check_resolutions ast "query" [ "LocalVar"; "LocalVar" ];
          (* Blocks close over enclosing locals: the assignment inside the
             block rebinds the method-local `acc` (one shared binding),
             it does not declare a fresh block-local. *)
          check_resolutions ast "acc" [ "LocalVar"; "LocalVar"; "LocalVar" ];
          check_single_binding ast "acc");
      t "crystal assignment shadows toplevel def" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/crystal/shadow_global_fn.cr")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Crystal ast;
          check_resolutions ast "query" [ "LocalVar"; "LocalVar" ];
          check_resolutions ast "acc" [ "LocalVar"; "LocalVar"; "LocalVar" ];
          check_single_binding ast "acc");
      t "php assignment shadows toplevel var" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/php/shadow_global_var.php")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Php ast;
          (* Occurrences: top-level `$counter = seed()` (Global), the
             function-local shadowing pair in shadows(), the
             `global $counter` rebinding pair in uses_global(). *)
          check_resolutions ast "$counter"
            [ "Global"; "LocalVar"; "LocalVar"; "Global"; "Global" ]);
      t "js bare assignment mutates outer binding" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/js/assign_outer.js")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Js ast;
          (* JS keeps the full-chain lookup: the bare assignment in setup()
             mutates the module-level `state`, it does not declare a local. *)
          check_resolutions ast "state" [ "Global"; "Global" ]);
    ]
