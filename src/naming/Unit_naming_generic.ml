open Common
open Fpath_.Operators

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = "tests"

(* Where a collected occurrence of a name is. *)
type occurrence =
  | In_expression
  | In_type
  | In_attribute (* an annotation or a decorator *)

(* The resolutions of every [occurrence] of [name], in source order. *)
let occurrence_resolutions (occurrence : occurrence)
    (ast : AST_generic.program) (name : string) :
    AST_generic.resolved_name option list =
  let acc = ref [] in
  let collect (s : string) (id_info : AST_generic.id_info) : unit =
    if String.equal s name then
      acc := !(id_info.AST_generic.id_resolved) :: !acc
  in
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_expr venv e =
        (match (occurrence, e.AST_generic.e) with
        | In_expression, AST_generic.N (AST_generic.Id ((s, _), id_info)) ->
            collect s id_info
        | _ -> ());
        super#visit_expr venv e

      method! visit_type_ venv ty =
        (match (occurrence, ty.AST_generic.t) with
        | In_type, AST_generic.TyN (AST_generic.Id ((s, _), id_info)) ->
            collect s id_info
        | _ -> ());
        super#visit_type_ venv ty

      method! visit_attribute venv attr =
        (match (occurrence, attr) with
        | ( In_attribute,
            AST_generic.NamedAttr (_, AST_generic.Id ((s, _), id_info), _) ) ->
            collect s id_info
        | _ -> ());
        super#visit_attribute venv attr
    end
  in
  visitor#visit_program () ast;
  List.rev !acc

(* The resolutions of every expression-level use of [name], in source
   order. Definition-site names (def/var entities, directives) are not
   expressions and are not collected. *)
let resolutions_of_name (ast : AST_generic.program) (name : string) =
  occurrence_resolutions In_expression ast name

let type_resolutions_of_name (ast : AST_generic.program) (name : string) =
  occurrence_resolutions In_type ast name

let attribute_resolutions_of_name (ast : AST_generic.program) (name : string)
    =
  occurrence_resolutions In_attribute ast name

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
      t "python base class of the same name refers to the earlier binding"
        (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path
                 "naming/python/class_extends_variable.py")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Python ast;
          let line (sid : AST_generic.SId.t) : int =
            let _, _, line, _ = AST_generic.SId.to_loc sid in
            line
          in
          (* Occurrences: the assignment, the base class, the call. *)
          match resolutions_of_name ast "Foo" with
          | [ Some (_, assigned); Some (_, base); Some (_, call) ] ->
              Alcotest.(check int) "the base refers to the assignment"
                (line assigned) (line base);
              Alcotest.(check int) "the call refers to the class" 4 (line call)
          | uses ->
              Alcotest.failf "expected three resolved uses of Foo, found %d"
                (List.length (List.filter Option.is_some uses)));
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
      t "java call of a static import refers to the import, not the class of \
         the same name"
        (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/java/static_import_and_class.java")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Java ast;
          (* the definition is the nested class; the use is the call in [f] *)
          match (def_sids_of_name ast "max", resolutions_of_name ast "max") with
          | [ class_ ], [ (Some (_, call) as use) ] ->
              Alcotest.(check string) "the call refers to the import"
                "ImportedEntity" (kind_of_resolution use);
              Alcotest.(check bool) "the call does not refer to the class" false
                (AST_generic.SId.equal class_ call)
          | definitions, uses ->
              Alcotest.failf
                "expected one definition and one use of max, found %d and %d"
                (List.length definitions) (List.length uses));
      t "java type parameter shadows the class of the same name" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path
                 "naming/java/type_parameter_and_class.java")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Java ast;
          (* the uses in a type are the return type of [get] and the type of
             the parameter of [put], both the class's type parameter *)
          match (def_sids_of_name ast "T", type_resolutions_of_name ast "T") with
          | [ class_ ], ([ _; _ ] as uses) ->
              uses
              |> List.iteri
                   (fun (i : int) (use : AST_generic.resolved_name option) ->
                     match use with
                     | Some (kind, sid) ->
                         Alcotest.(check string)
                           (spf "type use #%d of T is a type parameter" i)
                           "Parameter" (kind_of_resolution (Some (kind, sid)));
                         Alcotest.(check bool)
                           (spf "type use #%d of T is not the class" i)
                           false
                           (AST_generic.SId.equal class_ sid)
                     | None -> Alcotest.failf "type use #%d of T is unresolved" i)
          | definitions, uses ->
              Alcotest.failf
                "expected one definition and two type uses of T, found %d and \
                 %d"
                (List.length definitions) (List.length uses));
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
      t "go local type is in scope in its own definition" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/go/recursive_local_type.go")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Go ast;
          (* the uses in a type are [*Node] in the struct and the type of [n] *)
          match
            (def_sids_of_name ast "Node", type_resolutions_of_name ast "Node")
          with
          | [ _variable; type_ ], [ Some (_, field); Some (_, local) ] ->
              Alcotest.(check bool) "the field's type refers to the type" true
                (AST_generic.SId.equal type_ field);
              Alcotest.(check bool) "the variable's type refers to the type" true
                (AST_generic.SId.equal type_ local)
          | definitions, uses ->
              Alcotest.failf
                "expected two definitions and two type uses of Node, found %d \
                 and %d"
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
      t "c and c++ structure with its keyword refers to the structure, not the \
         function of the same name"
        (fun () ->
          (* the uses in a type are [struct stat *buf] and [struct stat st],
             the use in an expression is the call *)
          [ "naming/c/tag_and_function.c"; "naming/cpp/class_and_function.cpp" ]
          |> List.iter (fun (path : string) ->
                 let file = Fpath.v (Filename.concat tests_path path) in
                 let ast = parse_program file in
                 Naming_AST.resolve (Lang.lang_of_filename_exn file) ast;
                 match
                   ( def_sids_of_name ast "stat",
                     type_resolutions_of_name ast "stat",
                     resolutions_of_name ast "stat" )
                 with
                 | ( [ structure; function_ ],
                     [ Some (_, buf); Some (_, st) ],
                     [ Some (_, call) ] ) ->
                     Alcotest.(check bool)
                       (spf "%s: the parameter's type refers to the structure"
                          path)
                       true
                       (AST_generic.SId.equal structure buf);
                     Alcotest.(check bool)
                       (spf "%s: the variable's type refers to the structure" path)
                       true
                       (AST_generic.SId.equal structure st);
                     Alcotest.(check bool)
                       (spf "%s: the call refers to the function" path)
                       true
                       (AST_generic.SId.equal function_ call)
                 | definitions, types, expressions ->
                     Alcotest.failf
                       "%s: expected two definitions, two type uses and one call \
                        of stat, found %d, %d and %d"
                       path (List.length definitions) (List.length types)
                       (List.length expressions)));
      t "c name with the keyword struct refers to the tag, without it to the \
         typedef"
        (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/c/tag_and_typedef.c")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.C ast;
          (* the uses in a type are [struct node *next] in the structure, the
             typedef's [struct node], [node *head] and the local
             [struct node *next] *)
          match (def_sids_of_name ast "node", type_resolutions_of_name ast "node")
          with
          | ( [ structure; typedef ],
              [ Some (_, field); Some (_, aliased); Some (_, head); Some (_, local) ]
            ) ->
              Alcotest.(check bool) "the field's type refers to the structure" true
                (AST_generic.SId.equal structure field);
              Alcotest.(check bool) "the typedef refers to the structure" true
                (AST_generic.SId.equal structure aliased);
              Alcotest.(check bool) "the parameter's type refers to the typedef"
                true
                (AST_generic.SId.equal typedef head);
              Alcotest.(check bool) "the local's type refers to the structure" true
                (AST_generic.SId.equal structure local)
          | definitions, uses ->
              Alcotest.failf
                "expected two definitions and four type uses of node, found %d \
                 and %d"
                (List.length definitions) (List.length uses));
      t "c# type refers to the enum, not the property of the same name"
        (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/csharp/property_and_type.cs")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Csharp ast;
          (* the uses in a type include the property's type, the return type
             of [Pick] and the type of [chosen]; the use in an expression is
             the initialiser of [chosen] *)
          let refers_to (expected : AST_generic.SId.t) (what : string)
              (uses : AST_generic.resolved_name option list) : unit =
            uses
            |> List.iteri
                 (fun (i : int) (use : AST_generic.resolved_name option) ->
                   match use with
                   | Some (_, sid) ->
                       Alcotest.(check bool)
                         (spf "use #%d of Color refers to the %s" i what)
                         true
                         (AST_generic.SId.equal expected sid)
                   | None -> Alcotest.failf "use #%d of Color is unresolved" i)
          in
          match
            ( def_sids_of_name ast "Color",
              type_resolutions_of_name ast "Color",
              resolutions_of_name ast "Color" )
          with
          | [ enum; property ], (_ :: _ :: _ :: _ as types), (_ :: _ as expressions)
            ->
              refers_to enum "enum" types;
              refers_to property "property" expressions
          | definitions, types, expressions ->
              Alcotest.failf
                "expected two definitions, three type uses and an expression \
                 use of Color, found %d, %d and %d"
                (List.length definitions) (List.length types)
                (List.length expressions));
      t "c++ class in its own body refers to the class, not its constructor"
        (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/cpp/constructor_and_class.cpp")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Cpp ast;
          (* the uses in a type are the result and the parameter of [assign]
             and the result of [make]; the use in an expression is [Foo(1)] *)
          match
            ( def_sids_of_name ast "Foo",
              type_resolutions_of_name ast "Foo",
              resolutions_of_name ast "Foo" )
          with
          | ( [ class_; constructor ],
              ([ _; _; _ ] as types),
              [ Some (_, construction) ] ) ->
              Alcotest.(check bool) "the constructor refers to the class" true
                (AST_generic.SId.equal class_ constructor);
              types
              |> List.iteri
                   (fun (i : int) (use : AST_generic.resolved_name option) ->
                     match use with
                     | Some (_, sid) ->
                         Alcotest.(check bool)
                           (spf "type use #%d of Foo refers to the class" i)
                           true
                           (AST_generic.SId.equal class_ sid)
                     | None -> Alcotest.failf "type use #%d of Foo is unresolved" i);
              Alcotest.(check bool) "Foo(1) refers to the class" true
                (AST_generic.SId.equal class_ construction)
          | definitions, types, expressions ->
              Alcotest.failf
                "expected two definitions, three type uses and one expression \
                 use of Foo, found %d, %d and %d"
                (List.length definitions) (List.length types)
                (List.length expressions));
      t "java call refers to the method of the class's name, not the constructor"
        (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path
                 "naming/java/method_with_the_class_name.java")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Java ast;
          (* the definitions are the class, the method [void Foo()] and the
             constructor; the use is the call in [run] *)
          match (def_sids_of_name ast "Foo", resolutions_of_name ast "Foo") with
          | [ _class; method_; _constructor ], [ Some (_, call) ] ->
              Alcotest.(check bool) "the call refers to the method" true
                (AST_generic.SId.equal method_ call)
          | definitions, uses ->
              Alcotest.failf
                "expected three definitions and one use of Foo, found %d and %d"
                (List.length definitions) (List.length uses));
      t "c# class in its own body refers to the class, not its constructor"
        (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/csharp/class_in_its_body.cs")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Csharp ast;
          (* the use is [Item] in [Item.Total()], in the constructor *)
          match (def_sids_of_name ast "Item", resolutions_of_name ast "Item") with
          | [ class_; constructor ], [ Some (_, use) ] ->
              Alcotest.(check bool) "the constructor refers to the class" true
                (AST_generic.SId.equal class_ constructor);
              Alcotest.(check bool) "the use refers to the class" true
                (AST_generic.SId.equal class_ use)
          | definitions, uses ->
              Alcotest.failf
                "expected two definitions and one use of Item, found %d and %d"
                (List.length definitions) (List.length uses));
    ]
