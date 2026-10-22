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

(* The sid of the definition (def/class entity) named [name]. *)
let def_sid_of_name ast name =
  let acc = ref None in
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_definition venv ((ent, _) as def) =
        (match ent.AST_generic.name with
        | AST_generic.EN (AST_generic.Id ((s, _), id_info))
          when s = name && Option.is_none !acc -> (
            match !(id_info.AST_generic.id_resolved) with
            | Some (_, sid) -> acc := Some sid
            | None -> ())
        | _ -> ());
        super#visit_definition venv def
    end
  in
  visitor#visit_program () ast;
  !acc

let tests parse_program =
  Testo.categorize "naming generic"
    [
      t "switch value labels preserve the constant binding" (fun () ->
          [
            ("c", "c");
            ("cpp", "cpp");
            ("java", "java");
            ("js", "js");
            ("ts", "ts");
            ("php", "php");
            ("go", "go");
            ("dart", "dart");
            ("apex", "cls");
            ("crystal", "cr");
            ("swift", "swift");
          ]
          |> List.iter (fun (lang, ext) ->
              let file =
                Fpath.v
                  (spf "%s/rules/taint_switch_case_%s.%s"
                     tests_path lang ext)
              in
              let ast = parse_program file in
              Naming_AST.resolve (Lang.lang_of_filename_exn file) ast;
              match
                (def_sid_of_name ast "CMD", resolutions_of_name ast "CMD")
              with
              | Some def_sid, a :: b :: c :: d :: _ ->
                  (* Before, label, branch and after must all use the
                   * declaration. Later uses may explicitly shadow it. *)
                  [ a; b; c; d ]
                  |> List.iter (function
                    | Some (_, sid) ->
                        Alcotest.(check bool)
                          ("constant binding in " ^ ext)
                          true
                          (AST_generic.SId.equal def_sid sid)
                    | None -> Alcotest.failf "unresolved CMD in %s" ext)
              | _ -> Alcotest.failf "missing declaration or CMD uses in %s" ext));
      t "regression files" (fun () ->
          let dir = Filename.concat tests_path "naming/python" in
          let files1 = Common2.glob (spf "%s/*.py" dir) in
          let dir = Filename.concat tests_path "naming/go" in
          let files2 = Common2.glob (spf "%s/*.go" dir) in
          let dir = Filename.concat tests_path "naming/js" in
          let files3 = Common2.glob (spf "%s/*.js" dir) in
          let dir = Filename.concat tests_path "naming/java" in
          let files4 = Common2.glob (spf "%s/*.java" dir) in

          files1 @ files2 @ files3 @ files4
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
    ]
