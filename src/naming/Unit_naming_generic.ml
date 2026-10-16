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

(* The resolutions of every use of [name] as a type, in source order. *)
let type_resolutions_of_name ast name =
  let acc = ref [] in
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_type_ venv ty =
        (match ty.AST_generic.t with
        | AST_generic.TyN (AST_generic.Id ((s, _), id_info))
          when String.equal s name ->
            acc := !(id_info.AST_generic.id_resolved) :: !acc
        | _ -> ());
        super#visit_type_ venv ty
    end
  in
  visitor#visit_program () ast;
  List.rev !acc

(* The resolutions of every annotation or decorator [name], in source order. *)
let attribute_resolutions_of_name ast name =
  let acc = ref [] in
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_attribute venv attr =
        (match attr with
        | AST_generic.NamedAttr (_, AST_generic.Id ((s, _), id_info), _)
          when String.equal s name ->
            acc := !(id_info.AST_generic.id_resolved) :: !acc
        | _ -> ());
        super#visit_attribute venv attr
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

(* The sid of the first definition of [name] that has a binding. *)
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

(* The sids of every definition of [name] that has a binding, in order. *)
let def_sids_of_name ast name =
  let acc = ref [] in
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_definition venv ((ent, _) as def) =
        (match ent.AST_generic.name with
        | AST_generic.EN (AST_generic.Id ((s, _), id_info)) when s = name -> (
            match !(id_info.AST_generic.id_resolved) with
            | Some (_, sid) -> acc := sid :: !acc
            | None -> ())
        | _ -> ());
        super#visit_definition venv def
    end
  in
  visitor#visit_program () ast;
  List.rev !acc

(* No expression use of [name] binds the definition of that same name: an
   assignment target declares a variable, whatever scope it sits in (a
   top-level binding is still reported as [Global], so the resolution
   KIND cannot distinguish this — the binding identity can). *)
let check_uses_shadow_def ast name =
  match def_sid_of_name ast name with
  | None -> Alcotest.failf "no definition named '%s' found" name
  | Some def_sid ->
      resolutions_of_name ast name
      |> List.iteri (fun i resolution ->
             match resolution with
             | Some (_kind, sid) when AST_generic.SId.equal sid def_sid ->
                 Alcotest.failf
                   "use #%d of '%s' resolves to the definition (%s)" i name
                   (AST_generic.SId.to_string sid)
             | _ -> ())

(* All resolved uses of [name] refer to one and the same binding (sid). *)
let check_single_binding ast name =
  let sids =
    resolutions_of_name ast name
    |> List.filter_map (Option.map (fun (_kind, sid) -> sid))
    |> List.map AST_generic.SId.to_int
  in
  match sids with
  | [] -> Alcotest.failf "no resolved uses of '%s'" name
  | first :: rest ->
      rest
      |> List.iter (fun (sid : int) ->
             Alcotest.(check int)
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
      t "python class and function of one name follow source order" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/python/class_then_def.py")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Python ast;
          (* the function rebinds the class's name, and the call refers to it *)
          match (def_sids_of_name ast "Foo", resolutions_of_name ast "Foo") with
          | [ class_; function_ ], [ Some (_, call) ] ->
              Alcotest.(check bool) "the call refers to the function" true
                (AST_generic.SId.equal function_ call);
              Alcotest.(check bool) "the function rebinds the class's binding"
                true
                (Int.equal
                   (AST_generic.SId.to_int class_)
                   (AST_generic.SId.to_int function_))
          | definitions, uses ->
              Alcotest.failf
                "expected two definitions and one use of Foo, found %d and %d"
                (List.length definitions) (List.length uses));
      t "kotlin call refers to the class, not the parameter of the same name"
        (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/kotlin/class_and_parameter.kt")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Kotlin ast;
          match (def_sids_of_name ast "Foo", resolutions_of_name ast "Foo") with
          | [ class_ ], [ Some (_, call) ] ->
              Alcotest.(check bool) "the call refers to the class" true
                (AST_generic.SId.equal class_ call)
          | definitions, uses ->
              Alcotest.failf
                "expected one definition and one resolved use of Foo, found %d \
                 and %d"
                (List.length definitions) (List.length uses));
      t "python redefinition rebinds the same name" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/python/redefined_def.py")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Python ast;
          (* Two module-level definitions of [handler] are one binding,
             each at its own site; both calls refer to that binding. *)
          (match def_sids_of_name ast "handler" with
          | [ first; second ] ->
              Alcotest.(check bool) "one binding" true
                (AST_generic.SId.equal first second);
              Alcotest.(check bool) "two sites" false
                (Stdlib.( = )
                   (AST_generic.SId.to_loc first)
                   (AST_generic.SId.to_loc second))
          | sids ->
              Alcotest.failf "expected two definitions of handler, found %d"
                (List.length sids));
          check_single_binding ast "handler";
          (* the same bytes parsed twice get the same bindings *)
          let ast2 = parse_program file in
          Naming_AST.resolve Lang.Python ast2;
          Alcotest.(check (list int)) "deterministic bindings"
            (resolutions_of_name ast "handler"
            |> List.filter_map (Option.map (fun (_, sid) -> AST_generic.SId.to_int sid)))
            (resolutions_of_name ast2 "handler"
            |> List.filter_map (Option.map (fun (_, sid) -> AST_generic.SId.to_int sid))));
      t "python nested definitions bind in their function" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/python/nested_helpers.py")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Python ast;
          (* Two nested [helper]s in different functions are two bindings;
             each call refers to its own. *)
          (match def_sids_of_name ast "helper" with
          | [ first; second ] ->
              Alcotest.(check bool) "two bindings" false
                (AST_generic.SId.equal first second);
              Alcotest.(check (list int)) "each call to its own"
                [ AST_generic.SId.to_int first; AST_generic.SId.to_int second ]
                (resolutions_of_name ast "helper"
                |> List.filter_map
                     (Option.map (fun (_, sid) -> AST_generic.SId.to_int sid)))
          | sids ->
              Alcotest.failf "expected two definitions of helper, found %d"
                (List.length sids)));
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
      t "python destructuring assignment shadows module function" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path
                 "naming/python/shadow_global_fn_multi.py")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Python ast;
          (* Tuple target: both the target and the use are function-local,
             never the module-level [def query]. *)
          check_resolutions ast "query" [ "LocalVar"; "LocalVar" ];
          (* Augmented assignment writes the same local binding. *)
          check_resolutions ast "other"
            [ "LocalVar"; "LocalVar"; "LocalVar" ];
          check_single_binding ast "other");
      t "ruby multiple and toplevel assignment shadow defs" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path
                 "naming/ruby/shadow_global_fn_multi.rb")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Ruby ast;
          (* Method-scope multiple assignment declares a method-local; the
             top-level assignment declares a top-level binding (reported
             as [Global], the kind of every top-level scope entry). *)
          check_resolutions ast "query"
            [ "LocalVar"; "LocalVar"; "Global"; "Global" ];
          (* Neither binds the top-level [def query]: in Ruby locals and
             methods are separate namespaces, so assignment shadows. *)
          check_uses_shadow_def ast "query");
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
      t "php function body sees the file scope only through directives"
        (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/php/function_scope.php")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Php ast;
          (* Occurrences: the top-level assignment, the read in a function
             body without a directive, the arrow function's read, the
             closure's read through its [use], the read after an upper-case
             [GLOBAL]. *)
          check_resolutions ast "$config"
            [ "Global"; "Unresolved"; "Global"; "Global"; "Global" ];
          (* the directive creates the global *)
          check_resolutions ast "$created" [ "Global" ]);
      t "js bare assignment mutates outer binding" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/js/assign_outer.js")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Js ast;
          (* JS keeps the full-chain lookup: the bare assignment in setup()
             mutates the module-level `state`, it does not declare a local. *)
          check_resolutions ast "state" [ "Global"; "Global" ]);
      t "an annotation refers to the imported type, not the variable of the same name"
        (fun () ->
          [
            ("naming/java/annotation_and_field.java", "Inject");
            ("naming/kotlin/annotation_and_property.kt", "Inject");
          ]
          |> List.iter (fun ((path, name) : string * string) ->
                 let file = Fpath.v (Filename.concat tests_path path) in
                 let ast = parse_program file in
                 Naming_AST.resolve (Lang.lang_of_filename_exn file) ast;
                 Alcotest.(check (list string))
                   (spf "%s: resolutions of the annotation %s" path name)
                   [ "ImportedEntity" ]
                   (attribute_resolutions_of_name ast name
                   |> List.map kind_of_resolution)));
      t "a class or type definition does not resolve to the variable of the same name"
        (fun () ->
          (* a type alias next to a constant, and a nested class next to a
             field: the use refers to the constant or the field, and the
             alias or the class does not share its binding *)
          [
            ("naming/rust/type_alias_and_constant.rs", "T");
            ("naming/java/nested_class_and_field.java", "Inner");
            ("naming/kotlin/type_alias_and_variable.kt", "Foo");
          ]
          |> List.iter (fun ((path, name) : string * string) ->
                 let file = Fpath.v (Filename.concat tests_path path) in
                 let ast = parse_program file in
                 Naming_AST.resolve (Lang.lang_of_filename_exn file) ast;
                 match (def_sids_of_name ast name, resolutions_of_name ast name)
                 with
                 | variable :: others, [ Some (_kind, use) ] ->
                     Alcotest.(check bool)
                       (spf "the use of %s refers to the variable" name)
                       true
                       (AST_generic.SId.equal variable use);
                     Alcotest.(check bool)
                       (spf "no other definition of %s shares its binding" name)
                       true
                       (List.for_all
                          (fun (other : AST_generic.SId.t) ->
                            not (AST_generic.SId.equal variable other))
                          others)
                 | definitions, uses ->
                     Alcotest.failf
                       "%s: expected a resolved definition and one resolved \
                        use of %s, found %d and %d"
                       path name (List.length definitions)
                       (List.length (List.filter Option.is_some uses))));
      t "a use in a type refers to the type, not the variable of the same name"
        (fun () ->
          (* the definitions are the variable, then the type; in Java the
             uses in a type are a return type and the type of [new Inner()],
             in Kotlin the type of [copy] *)
          [
            ("naming/java/nested_class_and_field.java", "Inner", 2);
            ("naming/kotlin/type_alias_and_variable.kt", "Foo", 1);
          ]
          |> List.iter (fun ((path, name, count) : string * string * int) ->
                 let file = Fpath.v (Filename.concat tests_path path) in
                 let ast = parse_program file in
                 Naming_AST.resolve (Lang.lang_of_filename_exn file) ast;
                 let uses = type_resolutions_of_name ast name in
                 match def_sids_of_name ast name with
                 | [ _variable; type_ ] when Int.equal (List.length uses) count
                   ->
                     uses
                     |> List.iteri
                          (fun (i : int) (use : AST_generic.resolved_name option)
                          ->
                            match use with
                            | Some (_kind, sid) ->
                                Alcotest.(check bool)
                                  (spf "type use #%d of %s refers to the type" i
                                     name)
                                  true
                                  (AST_generic.SId.equal type_ sid)
                            | None ->
                                Alcotest.failf
                                  "%s: type use #%d of %s is unresolved" path i
                                  name)
                 | definitions ->
                     Alcotest.failf
                       "%s: expected two definitions and %d type uses of %s, \
                        found %d and %d"
                       path count name (List.length definitions)
                       (List.length uses)));
      t "python class binds its name in its scope" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/python/class_in_function.py")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Python ast;
          (* Occurrences: the global assignment, the call of the class in
             f, the read of the global in g. *)
          check_resolutions ast "Foo" [ "Global"; "LocalVar"; "Global" ];
          match (def_sids_of_name ast "Foo", resolutions_of_name ast "Foo") with
          | [ class_ ], [ Some (_, global); Some (_, call); Some (_, read) ] ->
              Alcotest.(check bool) "the call refers to the class" true
                (AST_generic.SId.equal class_ call);
              Alcotest.(check bool) "the read refers to the global" true
                (AST_generic.SId.equal global read)
          | definitions, uses ->
              Alcotest.failf
                "expected one class and three uses of Foo, found %d and %d"
                (List.length definitions) (List.length uses));
      t "go type binds its name in its block" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/go/type_in_function.go")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Go ast;
          (* Occurrences: the conversion to the type in f, the read of the
             package variable in g. *)
          check_resolutions ast "T" [ "LocalVar"; "Global" ];
          match (def_sids_of_name ast "T", resolutions_of_name ast "T") with
          | [ variable; type_ ], [ Some (_, conversion); Some (_, read) ] ->
              Alcotest.(check bool) "the conversion refers to the type" true
                (AST_generic.SId.equal type_ conversion);
              Alcotest.(check bool) "the read refers to the variable" true
                (AST_generic.SId.equal variable read)
          | definitions, uses ->
              Alcotest.failf
                "expected two definitions and two uses of T, found %d and %d"
                (List.length definitions) (List.length uses));
    ]
