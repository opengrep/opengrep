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
        | AST_generic.N (AST_generic.Id ((s, _), id_info))
        | AST_generic.N
            (AST_generic.IdQualified
              { name_last = (s, _), _; name_info = id_info; _ })
          when s = name ->
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

(* The sids of every definition (def/class entity) named [name], in order. *)
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

(* Every use of [name] binds the [n]th definition of that name (from 0, in
   source order). *)
let check_uses_bind_nth_def ast name n =
  match List.nth_opt (def_sids_of_name ast name) n with
  | None -> Alcotest.failf "no definition #%d named '%s' found" n name
  | Some def_sid -> (
      match resolutions_of_name ast name with
      | [] -> Alcotest.failf "no uses of '%s'" name
      | resolutions ->
          resolutions
          |> List.iteri (fun i resolution ->
                 match resolution with
                 | Some (_kind, sid) when AST_generic.SId.equal sid def_sid -> ()
                 | _ ->
                     Alcotest.failf "use #%d of '%s' does not bind definition #%d"
                       i name n))

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

(* Each use of [name] numbered by its binding, in order of first
   appearance; -1 for an unresolved use. *)
let binding_groups ast name : int list =
  let seen = Hashtbl.create 8 in
  resolutions_of_name ast name
  |> List.map (function
       | None -> -1
       | Some (_kind, sid) -> (
           let key = AST_generic.SId.to_int sid in
           match Hashtbl.find_opt seen key with
           | Some group -> group
           | None ->
               let group = Hashtbl.length seen in
               Hashtbl.add seen key group;
               group))

let check_binding_groups ast name expected =
  Alcotest.(check (list int))
    (spf "bindings of '%s'" name)
    expected (binding_groups ast name)

let check_sites_follow_textual_order ast name ~(last_use_sees_def : bool) =
  check_single_binding ast name;
  match (def_sid_of_name ast name, resolutions_of_name ast name) with
  | Some def_sid, [ Some (_, assigned); Some (_, last) ] ->
      let def_site = AST_generic.SId.to_loc def_sid in
      Alcotest.(check bool)
        (spf "the assignment to '%s' is at its own site" name)
        false
        (Stdlib.( = ) (AST_generic.SId.to_loc assigned) def_site);
      Alcotest.(check bool)
        (spf "the last use of '%s' sees the definition" name)
        last_use_sees_def
        (Stdlib.( = ) (AST_generic.SId.to_loc last) def_site)
  | _ -> Alcotest.failf "expected a definition and two resolved uses of '%s'" name

let name_resolutions_of_name ast name =
  let acc = ref [] in
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_name venv n =
        (match n with
        | AST_generic.Id ((s, _), id_info) when s = name ->
            acc := !(id_info.AST_generic.id_resolved) :: !acc
        | _ -> ());
        super#visit_name venv n
    end
  in
  visitor#visit_program () ast;
  List.rev !acc

let check_single_site ast name =
  check_single_binding ast name;
  match
    resolutions_of_name ast name
    |> List.filter_map (Option.map (fun (_, sid) -> AST_generic.SId.to_loc sid))
  with
  | [] -> Alcotest.failf "no resolved uses of '%s'" name
  | first :: rest ->
      rest
      |> List.iter (fun site ->
             Alcotest.(check bool)
               (spf "all uses of '%s' carry one site" name)
               true (Stdlib.( = ) first site))

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
          check_resolutions ast "jsonlib" [ "LocalVar"; "LocalVar" ]);
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
      t "go short variable declarations follow Go's scopes" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/go/short_var_scopes.go")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Go ast;
          (* [ctx, span := start(ctx)] reuses the parameter: the parameters
             and the body's top level are one scope. *)
          check_resolutions ast "ctx" [ "Parameter"; "Parameter"; "Parameter" ];
          (* [a, err := g()] declares err; [if err := h(); ...] declares a
             second err local to the if; [b, err := k()] reuses the first. *)
          check_binding_groups ast "err" [ 0; 0; 0; 1; 1; 1; 0; 0; 0 ];
          (* a top-level declaration group is not a scope *)
          check_resolutions ast "ga" [ "Global" ];
          check_resolutions ast "gb" [ "Global" ];
          (* a labelled declaration declares in the enclosing block *)
          check_resolutions ast "e" [ "LocalVar" ];
          check_resolutions ast "e2" [ "LocalVar" ]);
      t "go rest parameters bind their uses, not a field of the same name" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/go/rest_params_and_fields.go")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Go ast;
          check_resolutions ast "ctx"
            [ "Parameter"; "Parameter"; "Parameter"; "Parameter" ];
          check_single_binding ast "ctx");
      t "python star parameters bind their uses" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/python/rest_params.py")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Python ast;
          check_resolutions ast "args" [ "Global"; "Parameter" ];
          check_resolutions ast "kwargs" [ "Parameter" ]);
      t "java static fields are globals, instance fields class members"
        (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path
                 "naming/java/static_and_instance_fields.java")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Java ast;
          check_resolutions ast "s" [ "Global"; "Global" ];
          check_single_binding ast "s";
          check_resolutions ast "g" [ "Other"; "Other" ];
          check_single_binding ast "g");
      t "csharp static fields are globals, instance fields class members"
        (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path
                 "naming/csharp/static_and_instance_fields.cs")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Csharp ast;
          check_resolutions ast "s" [ "Global"; "Global" ];
          check_single_binding ast "s";
          check_resolutions ast "g" [ "Other"; "Other" ];
          check_single_binding ast "g");
      t "js and ts class members are reached only through this" (fun () ->
          [ ("naming/js/member_not_in_scope.js", Lang.Js);
            ("naming/js/member_not_in_scope.ts", Lang.Ts) ]
          |> List.iter (fun (path, lang) ->
                 let file = Fpath.v (Filename.concat tests_path path) in
                 let ast = parse_program file in
                 Naming_AST.resolve lang ast;
                 (* [x] in the method is the module's [x], not the field *)
                 check_resolutions ast "x" [ "Global"; "Global" ];
                 check_single_binding ast "x"));
      t "js and ts static members are reached only through the class" (fun () ->
          [ ("naming/js/static_member_not_in_scope.js", Lang.Js);
            ("naming/js/static_member_not_in_scope.ts", Lang.Ts) ]
          |> List.iter (fun (path, lang) ->
                 let file = Fpath.v (Filename.concat tests_path path) in
                 let ast = parse_program file in
                 Naming_AST.resolve lang ast;
                 (* [x] in the methods is the module's [x], not the static
                  * field, which is also a global *)
                 check_uses_bind_nth_def ast "x" 0));
      t "cpp capture lists refer to the enclosing variables" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/cpp/lambda_captures.cpp")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Cpp ast;
          (* [a] and [&b] are the enclosing variables inside the closure. *)
          check_resolutions ast "a" [ "Parameter"; "Parameter" ];
          check_single_binding ast "b";
          (* [y = b + 1] is a variable of the closure, not the outer [y]. *)
          check_binding_groups ast "y" [ 0; 1 ]);
      t "c switch body is one scope" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/c/switch_scope.c")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.C ast;
          check_resolutions ast "y" [ "LocalVar"; "LocalVar"; "LocalVar"; "LocalVar" ];
          check_single_binding ast "y");
      t "java switch body is one scope" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/java/switch_scope.java")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Java ast;
          check_resolutions ast "y" [ "LocalVar"; "LocalVar"; "LocalVar" ];
          check_single_binding ast "y");
      t "csharp members, declarators, using resources and switch" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/csharp/members_and_scopes.cs")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Csharp ast;
          (* fields with several declarators, a property and an event are
             class members; locals with several declarators, using
             resources and a declaration in one case are locals *)
          check_resolutions ast "fa" [ "Other" ];
          check_resolutions ast "fb" [ "Other" ];
          check_resolutions ast "P" [ "Other"; "Other"; "Other" ];
          check_resolutions ast "E" [ "Other" ];
          check_resolutions ast "a" [ "LocalVar" ];
          check_resolutions ast "b" [ "LocalVar" ];
          check_resolutions ast "r" [ "LocalVar" ];
          check_resolutions ast "s" [ "LocalVar" ];
          check_resolutions ast "y" [ "LocalVar"; "LocalVar"; "LocalVar" ];
          check_single_binding ast "y");
      t "apex fields, property and locals" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/apex/members.cls")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Apex ast;
          check_resolutions ast "fa" [ "Other" ];
          check_resolutions ast "fb" [ "Other" ];
          check_resolutions ast "P" [ "Other" ];
          check_resolutions ast "a" [ "LocalVar" ];
          check_resolutions ast "b" [ "LocalVar" ]);
      t "vb.net property resolves in methods" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/vb/property.vb")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Vb ast;
          check_resolutions ast "fa" [ "Other"; "Other"; "Other" ];
          check_resolutions ast "P" [ "Other" ]);
      t "dart class fields and switch cases" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/dart/fields_and_switch.dart")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Dart ast;
          check_resolutions ast "fa" [ "Other" ];
          check_resolutions ast "fb" [ "Other" ];
          check_resolutions ast "a" [ "LocalVar" ];
          check_resolutions ast "b" [ "LocalVar" ];
          (* a case's statements share one scope *)
          check_resolutions ast "c" [ "LocalVar" ];
          check_resolutions ast "d" [ "LocalVar" ]);
      t "swift class fields resolve in methods" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/swift/fields.swift")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Swift ast;
          check_resolutions ast "fa" [ "Other" ];
          check_resolutions ast "fb" [ "Other" ];
          check_resolutions ast "fc" [ "Other" ]);
      t "solidity state variables resolve in functions" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/solidity/fields.sol")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Solidity ast;
          check_resolutions ast "fa" [ "Other" ];
          check_resolutions ast "fb" [ "Other" ]);
      t "kotlin do-while condition sees the body" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/kotlin/do_while.kt")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Kotlin ast;
          check_resolutions ast "x" [ "LocalVar" ]);
      t "elixir parenthesised sequence is not a scope" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/elixir/parenthesised.ex")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Elixir ast;
          check_resolutions ast "a" [ "LocalVar" ];
          check_resolutions ast "b" [ "LocalVar" ];
          (* a binding inside if does not leak *)
          check_resolutions ast "c" [ "Unresolved" ]);
      t "cpp call with type arguments binds the function" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/cpp/template_call.cpp")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Cpp ast;
          check_uses_bind_nth_def ast "f" 0);
      t "cpp rooted and qualified calls with type arguments" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/cpp/qualified_calls.cpp")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Cpp ast;
          check_resolutions ast "f" [ "Global"; "LocalVar" ];
          check_binding_groups ast "f" [ 0; 1 ];
          check_uses_bind_nth_def ast "g" 0);
      t "kotlin callable references bind the function and the class" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/kotlin/callable_reference.kt")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Kotlin ast;
          check_uses_bind_nth_def ast "f" 0;
          check_uses_bind_nth_def ast "Foo" 0);
      t "lua assignment to an undeclared name binds a global" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/lua/global_assign.lua")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Lua ast;
          check_resolutions ast "f" [ "Global"; "Global" ];
          check_single_binding ast "f";
          check_resolutions ast "M" [ "Global"; "Global"; "Global" ];
          check_single_binding ast "M";
          check_resolutions ast "count" [ "Global"; "Global" ];
          check_single_binding ast "count";
          check_resolutions ast "n" [ "LocalVar"; "LocalVar" ];
          check_single_binding ast "n";
          check_resolutions ast "later" [ "Global"; "Global" ];
          check_single_binding ast "later";
          check_resolutions ast "print" [ "Unresolved" ]);
      t "julia dotted definition resolves its module" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/julia/dotted_definition.jl")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Julia ast;
          check_resolutions ast "Foo" [ "Global"; "Global" ];
          check_uses_bind_nth_def ast "Foo" 0);
      t "r assignment binds in the current function" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/r/assign_scopes.R")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.R ast;
          check_resolutions ast "x" [ "Global"; "LocalVar"; "LocalVar"; "Global" ];
          check_binding_groups ast "x" [ 0; 1; 1; 0 ];
          check_resolutions ast "f" [ "Global"; "Global" ];
          check_single_binding ast "f");
      t "hack assignment binds a function local" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/hack/function_locals.hack")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Hack ast;
          check_resolutions ast "$x" [ "LocalVar"; "LocalVar"; "LocalVar"; "LocalVar" ];
          check_single_binding ast "$x");
      t "php namespaces hold their definitions" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/php/namespaces.php")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Php ast;
          check_uses_bind_nth_def ast "LIMIT" 0;
          match def_sids_of_name ast "helper" with
          | [ in_app; in_global ] ->
              Alcotest.(check bool) "two bindings" false
                (AST_generic.SId.equal in_app in_global);
              Alcotest.(check (list int)) "each use to its namespace's definition"
                [ AST_generic.SId.to_int in_app; AST_generic.SId.to_int in_global ]
                (resolutions_of_name ast "helper"
                |> List.filter_map
                     (Option.map (fun (_, sid) -> AST_generic.SId.to_int sid)))
          | sids ->
              Alcotest.failf "expected two definitions of helper, found %d"
                (List.length sids));
      t "hack namespaces hold their definitions" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/hack/namespace.hack")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Hack ast;
          check_binding_groups ast "helper" [ 0; 1 ]);
      t "python class rebinds a function of the same name" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/python/function_then_class.py")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Python ast;
          check_resolutions ast "make" [ "Global"; "Other" ];
          check_single_binding ast "make");
      t "python binding statements rebind in textual order" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/python/call_before_definition.py")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Python ast;
          check_sites_follow_textual_order ast "checker" ~last_use_sees_def:true;
          check_sites_follow_textual_order ast "handler" ~last_use_sees_def:false;
          check_resolutions ast "counter" [ "Global"; "Global"; "Global"; "Global" ];
          check_single_site ast "counter");
      t "bash assignment is global unless a local declaration is in force"
        (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/bash/assign_scopes.bash")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Bash ast;
          check_resolutions ast "y" [ "LocalVar"; "LocalVar" ];
          check_single_binding ast "y";
          check_resolutions ast "z" [ "Global"; "Global"; "Global" ];
          check_single_binding ast "z";
          check_resolutions ast "x" [ "Global"; "Global"; "Global" ];
          check_single_binding ast "x");
      t "julia assignment in a function declares a local unless global"
        (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/julia/assign_scopes.jl")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Julia ast;
          check_resolutions ast "x" [ "Global"; "LocalVar"; "LocalVar"; "Global" ];
          check_binding_groups ast "x" [ 0; 1; 1; 0 ]);
      t "rust Self in an impl block is the implemented type" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/rust/self_type.rs")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Rust ast;
          match def_sid_of_name ast "Foo" with
          | None -> Alcotest.failf "no definition of Foo"
          | Some foo ->
              let foo = Some (AST_generic.SId.to_int foo) in
              Alcotest.(check (list (option int)))
                "Self is Foo in the impl blocks and unbound in the trait"
                [ foo; foo; None; foo; foo ]
                (name_resolutions_of_name ast "Self"
                |> List.map
                     (Option.map (fun (_, sid) -> AST_generic.SId.to_int sid))));
      t "elixir module body is a scope" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/elixir/modules.ex")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Elixir ast;
          (match def_sids_of_name ast "f" with
          | [ in_handler; in_other ] ->
              Alcotest.(check bool) "two bindings" false
                (AST_generic.SId.equal in_handler in_other)
          | sids ->
              Alcotest.failf "expected two definitions of f, found %d"
                (List.length sids));
          check_uses_bind_nth_def ast "f" 0);
      t "ruby module body is a scope" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/ruby/modules.rb")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Ruby ast;
          (match def_sids_of_name ast "from_a" with
          | [ in_from_a; in_not_included ] ->
              Alcotest.(check bool) "two bindings" false
                (AST_generic.SId.equal in_from_a in_not_included)
          | sids ->
              Alcotest.failf "expected two definitions of from_a, found %d"
                (List.length sids));
          check_resolutions ast "LIMIT" [ "Global"; "Global" ];
          check_single_binding ast "LIMIT");
      t "java overloads share one binding, each at its own site" (fun () ->
          let file =
            Fpath.v (Filename.concat tests_path "naming/java/overloads.java")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Java ast;
          (match def_sids_of_name ast "g" with
          | [ first; second ] ->
              Alcotest.(check bool) "one binding" true
                (AST_generic.SId.equal first second);
              Alcotest.(check bool) "two sites" false
                (AST_generic.SId.same_site first second)
          | sids ->
              Alcotest.failf "expected two definitions of g, found %d"
                (List.length sids));
          check_uses_bind_nth_def ast "g" 0);
      t "lua binding statements rebind in textual order" (fun () ->
          let file =
            Fpath.v
              (Filename.concat tests_path "naming/lua/call_before_definition.lua")
          in
          let ast = parse_program file in
          Naming_AST.resolve Lang.Lua ast;
          check_sites_follow_textual_order ast "checker" ~last_use_sees_def:true;
          check_sites_follow_textual_order ast "handler" ~last_use_sees_def:false);
    ]
