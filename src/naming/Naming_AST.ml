(* Yoann Padioleau, Iago Abal
 *
 * Copyright (C) 2020-2022 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open AST_generic
open Naming_utils
module H = AST_generic_helpers
module Log = Log_naming.Log

(* see error() below *)
let error_report = false

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal of this module is to resolve names, a.k.a naming or
 * scope resolution, and to do it in a generic way on the generic AST.
 * update: this module is also (ab)used to propagate type information
 * used in semgrep for matching typed metavariables.
 *
 * In a compiler you often have those phases:
 *  - lexing
 *  - parsing
 *  - naming (the goal of this file)
 *  - typing
 *  - intermediate code generation
 *  - optimizing
 *  - ...
 *
 * The goal of the naming phase is to simplify following phases by having
 * each use of an entity clearly linked to its definition. For example,
 * when you see in the AST the use of the identifier 'a', this 'a'
 * could reference a local variable, or a parameter, or a global,
 * or a global defined in another module but imported in the current
 * namespace, or a variable defined in a nested block that "shadows" an
 * enclosing variable with the same name.
 * By resolving once and for all all uses of an entity to its definition,
 * for example by renaming some shadow variables (see AST_generic.gensym),
 * we simpify further phases, which don't have to maintain anymore a
 * complex environment to deal with scoping issues
 * (see the essence Of Python paper "Python: The Full Monty" where they
 * show that even complex IDEs still do not correctly handle Python
 * scoping rules and perform wrong renaming refactorings).
 *
 * Resolving names by tagging identifiers is also useful for
 * codemap/efuns to colorize identifiers (locals, params, globals, unknowns)
 * differently.
 *
 * alternatives:
 *  - CURRENT: generic naming and use of a 'ref resolved_name' to annotate
 *    the generic AST. Note that the use of a ref that can be shared with
 *    the lang-specific AST (e.g., ast_go.ml) allows tools like codemap/efuns
 *    to benefit from the generic naming analysis while still caring only
 *    about the lang-specific AST (even though we may want at some point
 *    to have a generic highlighter).
 *  - define a separate type for a named ast, e.g., nast.ml (as done in
 *    hack/skip) instead of modifying refs, with a unique identifier
 *    for each entity. However, this is tedious to
 *    write as both types are almost identical (maybe a functor could help,
 *    or a generic aast type as in recent hack code). Moreover, this is really
 *    useful for complex global analysis (or at least semi-global as in
 *    OCaml where you still need to open many .cmi when you locally type a .ml)
 *    such as typing where we want to resolve every use of a global.
 *    For semgrep, where we might for quite some time restrict ourselves to
 *    local analysis, maybe the ref implementation technique is good enough.
 *  - implement a resolve_xxx.ml for each language instead of doing it
 *    on the generic AST. That is what I was doing previously, which
 *    has some advantages (some language-specific constructs that introduce
 *    new variables, for example Python comprehensions, are hard to analyze
 *    once converted to the generic AST because they are under an
 *    Other_xxx category)
 *    update: actually comprehensions are now a regular AST element
 *    However, there's potentially lots of code
 *    duplication for each language and it's easy for a language to fall
 *    behind.
 *    A nice compromise might be to do most of the work in naming_ast.ml
 *    but still have lang-specific resolve_xxx.ml to tag special
 *    constructs that override what naming_ast.ml would do.
 *    See set_resolved()
 *
 * TODO:
 *  - generalize the original "resolvers":
 *    * resolve_go.ml
 *    * resolve_python.ml
 *    * ast_js_build.ml
 *    * check_variables_cpp.ml
 *    * check_variables_php.ml
 *  - introduce extra VarDef for languages that do not have them like
 *    Python/PHP where the first use is a def (which in turn requires
 *    special construct like 'global' or 'nonlocal' to disable this).
 *  - go:
 *    * handle DShortVars and Foreach local vars, DMethod receiver parameter,
 *      and TypeName for new types
 *    * in theory if/for/switch with their init declare new scope, as well
 *      as Block
 *    * should do first pass to get all toplevel decl as you can use
 *      forward ref in Go
 *  - get rid of the original "resolvers":
 *    * resolve_xxx.ml
 *    * ast_js_build.ml
 *    * check_variables_xxx.ml
 *  - get rid of or unify scope_code.ml, scope_php.ml, and
 *    ast_generic.resolved_name
 *  - resolve also types! in java if you import org.foo.Bar then later
 *    you can simply use Bar x; for a type, but we don't currently resolve
 *    those.
 *
 * history:
 *  - PHP deadcode detector with global analysis and global code database
 *  - local name resolution for PHP and C/C++ in check_variables_cpp.ml and
 *    check_variables_php.ml for codemap semantic highlighting of identifiers
 *    (mainly local vs params vs globals vs unknown) and for checkModule
 *    (scheck ancestor). Use of a ref for C/C++.
 *  - graph_code_xxx.ml global name resolution for PHP, then Java,
 *    then ML, then ML via cmt, then Clang ASTs, then C, then Javascript
 *  - separate named AST (nast.ml) and naming phase for Hack
 *  - local name resolution for code highlighting for Javascript, then Python
 *    to better colorize identifiers in codemap/efuns, but separate from
 *    a variable checker (resolve_xxx.ml instead of check_variables_xxx.ml)
 *  - AST generic and its resolved_name ref
 *  - simple resolve_python.ml with variable and import resolution
 *  - separate resolve_go.ml with import resolution
 *  - try to unify those resolvers in one file, naming_ast.ml
 *  - resolve names for OCaml constructs and factorize name resolution
 *    in better 'name' type and 'kname' hook.
 *)

(*****************************************************************************)
(* Scope *)
(*****************************************************************************)

(* this includes the "single unique id" (sid) *)
type resolved_name = AST_generic.resolved_name

type scope_info = {
  (* variable kind and sid *)
  entname : resolved_name;
  (* variable type, if known *)
  enttype : type_ option;
}

type scope = (string, scope_info) Assoc.t

type scopes = {
  global : scope ref;
  (* function, nested blocks, nested functions (lambdas) *)
  blocks : scope list ref;
  (* useful for python, kind of global scope but for entities *)
  imported : scope ref;
      (* todo?
       * - class? right now we abuse EnclosedVar in resolved_name_kind.
       * - function? for 'var' in JS
       *)
}

let default_scopes () = { global = ref []; blocks = ref []; imported = ref [] }

(* because we use a Visitor instead of a clean recursive
 * function passing down an environment, we need to emulate a scoped
 * environment by using save_excursion.
 *)

let with_new_function_scope params scopes f =
  Common.save_excursion_unsafe scopes.blocks (params :: !(scopes.blocks)) f

let with_new_block_scope scopes f =
  Common.save_excursion_unsafe scopes.blocks ([] :: !(scopes.blocks)) f

let add_ident_current_scope (s, _) resolved scopes =
  match !(scopes.blocks) with
  | [] -> scopes.global := (s, resolved) :: !(scopes.global)
  | xs :: xxs -> scopes.blocks := ((s, resolved) :: xs) :: xxs

(* for Python *)
let add_ident_imported_scope (s, _) resolved scopes =
  scopes.imported := (s, resolved) :: !(scopes.imported)

let add_ident_global_scope (s, _) resolved scopes =
  scopes.global := (s, resolved) :: !(scopes.global)

(* for JS 'var' *)
let _add_ident_function_scope _id _resolved _scopes = raise Todo
let untyped_ent name = { entname = name; enttype = None }

(* see also lookup_scope_opt below taking as a parameter the environment *)
let rec lookup ?(class_attr = false) s xxs =
  match xxs with
  | [] -> None
  | xs :: xxs -> (
      match List.assoc_opt s xs with
      | None -> lookup ~class_attr s xxs
      | Some res when class_attr -> (
          match res.entname with
          | EnclosedVar, _ -> Some res
          (* If we are looking for a class attribute, and we encounter something
           * else, e.g. a 'Parameter', then we should keep looking. This happens
           * e.g. in this situation:
           *
           *     class Test {
           *         private int x;
           *         public void test(int x) {
           *             foo(this.x);           // <--- this.x is not shadowed by
           *                                    //      the `x` parameter.
           *         }
           *     }
           *)
          | __else__ -> lookup ~class_attr s xxs)
      | Some res -> Some res)

(* for Python, PHP *)
let lookup_global_scope (s, _) scopes = lookup s [ !(scopes.global) ]

(* for Python, PHP *)
let lookup_nonlocal_scope id scopes =
  let s, tok = id in
  match !(scopes.blocks) with
  | _ :: xxs -> lookup s xxs
  | [] ->
      let _ = error tok "no outerscope" in
      None

(* for a PHP closure [use]: the variable of the scope enclosing the closure,
 * the file's variables when the closure is at the top level *)
let lookup_enclosing_scope (s, _) scopes =
  match !(scopes.blocks) with
  | _ :: xxs -> lookup s (xxs @ [ !(scopes.global) ])
  | [] -> None

let has_block_scope (lang : Lang.t) =
  match lang with
  (* These languages don't have block scope *)
  | Ruby
  | Python
  | Php ->
      false
  | _js_ when Lang.is_js lang -> false
  (* The rest do. *)
  | _else_ -> true

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)
type context =
  | AtToplevel
  | InClass
  (* separate InMethod? InLambda? just look for InFunction::InClass::_ *)
  | InFunction

type env = {
  ctx : context list ref;
  (* handle locals/params/globals, block vas, enclosed vars (closures).
   * handle also basic typing information now for Java/Go.
   *)
  names : scopes;
  (* Inside a PHP function body, the number of enclosing block scopes it
   * hides: it sees no enclosing local and no file variable, only what a
   * [global] directive or a closure [use] plants in it. None outside any
   * function body, where the file scope is visible. *)
  hidden_blocks : int option ref;
  in_lvalue : bool ref;
  in_type : bool ref;
  lang : Lang.t;
  (* The real file being resolved.  A resolved name's sid is its definition
     token's place within this file (see [AST_generic.SId]); naming processes
     a single file, so the token's file and this one coincide. *)
  file : string;
}

let default_env lang file =
  {
    ctx = ref [ AtToplevel ];
    names = default_scopes ();
    hidden_blocks = ref None;
    in_lvalue = ref false;
    in_type = ref false;
    lang;
    file;
  }

(*****************************************************************************)
(* Environment Helpers *)
(*****************************************************************************)

let with_new_context ctx env f =
  Common.save_excursion_unsafe env.ctx (ctx :: !(env.ctx)) f

let top_context env =
  match !(env.ctx) with
  | [] -> raise Impossible
  | x :: _xs -> x

let set_resolved env id_info x =
  (* TODO? maybe do it only if we have something better than what the
   * lang-specific resolved found?
   *)
  id_info.id_resolved := Some x.entname;
  (* This is defensive programming against the possibility of introducing
   * cycles in the AST.
   * Indeed, when we are inside a type, especially in  (OtherType (OT_Expr)),
   * we don't want set_resolved to set the type on some Id because
   * this could lead to cycle in the AST because of id_type
   * that will reference a type, that could containi an OT_Expr, containing
   * an Id, that could contain the same id_type, and so on.
   * See tests/naming/python/shadow_name_type.py for a pathological example
   * See also tests/rust/parsing/misc_recursion.rs for another example.
   *)
  if not !(env.in_type) then id_info.id_type := x.enttype

(* the block scopes a lookup may see, innermost first *)
let visible_blocks env =
  let blocks = !(env.names.blocks) in
  match !(env.hidden_blocks) with
  | None -> blocks
  | Some hidden ->
      let visible = List.length blocks - hidden in
      List.filteri (fun (i : int) (_ : scope) -> i < visible) blocks

(* accessors *)
let lookup_scope_opt ?(class_attr = false) (s, _) env =
  let scopes = env.names in

  let actual_scopes =
    match !(scopes.blocks) with
    | [] -> [ !(scopes.global); !(scopes.imported) ]
    | xs :: xxs -> (
        match env.lang with
        | Lang.Python ->
            if
              !(env.in_lvalue)
              (* just look current scope! no access to nested scopes or global *)
            then [ xs; !(scopes.imported) ]
            else [ xs ] @ xxs @ [ !(scopes.global); !(scopes.imported) ]
        | Lang.Php ->
            (* just look current scope! no access to nested scopes or global:
             * a function body sees its own locals, what a [global] directive
             * or a closure [use] planted in it, the file's functions and
             * constants (no [$] sigil), not the file's variables; an arrow
             * function sees the scopes enclosing it, the file's variables
             * included when no function body is in between *)
            let file_scope =
              if
                Option.is_none !(env.hidden_blocks)
                || not (String.starts_with ~prefix:"$" s)
              then [ !(scopes.global) ]
              else []
            in
            visible_blocks env @ file_scope @ [ !(scopes.imported) ]
        | _ -> [ xs ] @ xxs @ [ !(scopes.global); !(scopes.imported) ])
  in
  lookup ~class_attr s actual_scopes

(* Decides whether an implicit assignment [x = e] rebinds an existing
 * variable (Some _) or declares a new one (None).
 *
 * Python: assignment makes a name function-local unless a [global] /
 * [nonlocal] directive binds it — and directives plant their resolution
 * in the current block scope (see the UseOuterDecl case) — so only the
 * current block scope (parameters, prior locals, directive entries)
 * suppresses the implicit declaration. A name that merely resolves in an
 * enclosing / global / imported scope is shadowed by the assignment
 * (e.g. a function-local [query = ...] under a module-level [def query]).
 * Exception: the rules ecosystem relies on flow-insensitive naming for
 * imports ([import pdb as db] then [db = "a string"] with later [db.Pdb]
 * uses still expected to match, cf. python/lang/correctness/pdb.yaml in
 * semgrep-rules), so an Imported* resolution anywhere on the chain still
 * suppresses the declaration.
 *
 * PHP: a function body sees nothing from enclosing scopes except what a
 * [global $x;] directive binds (planted in the current block scope by
 * the UseOuterDecl case) or a closure [use] captures, so only the
 * current block scope suppresses the declaration. Variables carry their
 * [$] sigil so they can never collide with function/import names; no
 * import exception is needed.
 *
 * Ruby / Crystal: blocks and procs close over enclosing locals, so an
 * assignment anywhere on the block chain rebinds them; top-level locals
 * live in the global scope and stay visible (script-style code). What
 * assignment does shadow is a same-named top-level [def]: defs live in
 * the imported scope, which is excluded here.
 *
 * Other implicit-declaration languages keep the full-chain lookup: a JS
 * bare assignment genuinely mutates the outer binding.
 *)
let lookup_for_implicit_assign_opt id env =
  let s, _ = id in
  match (env.lang, !(env.names.blocks)) with
  | Lang.Python, current_block :: _ -> (
      match lookup s [ current_block ] with
      | Some _ as resolved -> resolved
      | None -> (
          match lookup_scope_opt id env with
          | Some { entname = (ImportedEntity _ | ImportedModule _), _; _ } as
            resolved ->
              resolved
          | Some _
          | None ->
              None))
  | Lang.Php, current_block :: _ -> lookup s [ current_block ]
  | (Lang.Ruby | Lang.Crystal), blocks ->
      (* Blocks close over enclosing locals, and top-level locals stay
         visible; [blocks] is empty at the top level, where the chain is
         just the global scope.  The imported scope — where top-level
         [def]s live — is excluded either way: that is what assignment
         shadows (locals and methods are separate namespaces). *)
      lookup s (blocks @ [ !(env.names.global) ])
  | _ -> lookup_scope_opt id env

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let error tok s =
  if error_report then raise (Parsing_error.Other_error (s, tok))
  else Log.err (fun m -> m "%s at %s" s (Tok.stringpos_of_tok tok))

(*****************************************************************************)
(* Typing Helpers *)
(*****************************************************************************)

(* should use TyBuiltin instead? *)
let make_type type_string tok =
  Some (TyN (Id ((type_string, tok), empty_id_info ())) |> AST_generic.t)

(* This is only one part of the code to handle typed metavariables. Here
 * the goal is to help is setting the id_info.id_type for a few
 * identifiers in VarDef or Assign. Then, Generic_vs_generic.m_compatible_type
 * can leverage the info.
 *)
let rec get_resolved_type lang (vinit, vtype) =
  match vtype with
  | None
  | Some { t = TyAny _; _ } -> (
      (* Currently these vary between languages *)
      (* Alternative is to define a TyInt, TyBool, etc. in the generic AST *)
      (* so this would be more portable across languages *)
      match vinit with
      | Some { e = L (Bool (_, tok)); _ } -> make_type "bool" tok
      | Some { e = L (Int (_, tok)); _ } -> make_type "int" tok
      | Some { e = L (Float (_, tok)); _ } -> make_type "float" tok
      | Some { e = L (Char (_, tok)); _ } -> make_type "char" tok
      | Some { e = L (String (_, (_, tok), _)); _ } ->
          let string_str =
            match lang with
            | Lang.Go -> "str"
            | Lang.Js
            | Lang.Ts ->
                "string"
            | _ -> "string"
          in
          make_type string_str tok
      | Some { e = L (Regexp ((_, (_, tok), _), _)); _ } ->
          make_type "regexp" tok
      | Some { e = RegexpTemplate ((l, _fragments, _r), _); _ } ->
          (* TODO: need proper location instead of just the opening '/'? *)
          make_type "regexp" l
      | Some { e = L (Unit tok); _ } -> make_type "unit" tok
      | Some { e = L (Null tok); _ } -> make_type "null" tok
      | Some { e = L (Imag (_, tok)); _ } -> make_type "imag" tok
      (* alt: lookup id in env to get its type, which would be cleaner *)
      | Some { e = N (Id (_, { id_type; _ })); _ } -> !id_type
      | Some { e = New (_, tp, _, (_, _, _)); _ } -> Some tp
      (* Scala companion-object apply: [Map(...)],
       * [mutable.Map[K, V]()], [HashMap[K, V]()]. The head of
       * the callee gives the type; when the call is
       * parameterised, we preserve the type arguments as
       * [TyApply]. Scala-gated so other languages' inference
       * paths are untouched; non-Map capitalised heads (e.g.
       * [List(...)]) are harmless — the library-call
       * recognisers gate against the Map family list. *)
      | Some { e = Call (callee, _); _ } when lang =*= Lang.Scala ->
          let name_of_simple_expr (e : expr) =
            match e.e with
            | N (Id (id, _)) -> Some id
            | N (IdQualified { name_last = id, _; _ }) -> Some id
            | DotAccess (_, _, FN (Id (id, _))) -> Some id
            | DotAccess
                (_, _, FN (IdQualified { name_last = id, _; _ })) ->
                Some id
            | _ -> None
          in
          let head_name_and_targs (e : expr) =
            match e.e with
            | OtherExpr (("InstanciatedExpr", _), E inner :: rest) ->
                let targs =
                  List.filter_map
                    (function T t -> Some t | _ -> None)
                    rest
                in
                Option.map
                  (fun id -> (id, targs))
                  (name_of_simple_expr inner)
            | _ ->
                Option.map (fun id -> (id, [])) (name_of_simple_expr e)
          in
          (match head_name_and_targs callee with
           | Some ((s, tok), []) when String_.is_capitalized s ->
               make_type s tok
           | Some ((s, tok), targs) when String_.is_capitalized s ->
               let head_ty =
                 TyN (Id ((s, tok), empty_id_info ())) |> AST_generic.t
               in
               let args =
                 Tok.unsafe_fake_bracket (List.map (fun t -> TA t) targs)
               in
               Some (TyApply (head_ty, args) |> AST_generic.t)
           | _ -> None)
      | Some { e = Ref (tok, exp); _ } ->
          Option.bind
            (get_resolved_type lang (Some exp, None))
            (fun x -> Some (t @@ TyPointer (tok, x)))
      | _ -> None)
  | Some _ -> vtype

(*****************************************************************************)
(* Other Helpers *)
(*****************************************************************************)

let is_resolvable_name_ctx env lang =
  match top_context env with
  | AtToplevel
  | InFunction ->
      true
  | InClass -> (
      match lang with
      (* true for Java so that we can type class fields *)
      | Lang.Java
      | Lang.Kotlin
      | Lang.Apex
      | Lang.Csharp
      | Lang.Vb
      (* true for JS/TS so that we can resolve class methods *)
      | Lang.Js
      | Lang.Ts
      | Lang.Php
      | Lang.Scala
      | Lang.C
      | Lang.Cpp ->
          true
      | _ -> false)

let resolved_name_kind env lang =
  match top_context env with
  | AtToplevel -> Global
  | InFunction -> LocalVar
  | InClass -> (
      match lang with
      (* true for Java so that we can type class fields.
       * alt: use a different scope.class?
       *)
      | Lang.Java
      | Lang.Kotlin
      | Lang.Apex
      | Lang.Csharp
      | Lang.Vb
      (* true for JS/TS to resolve class methods. *)
      | Lang.Js
      | Lang.Ts
      | Lang.Php
      | Lang.Scala
      | Lang.C (* can happen for macros inside structs *)
      | Lang.Cpp ->
          EnclosedVar
      | _ -> raise Impossible)

(* !also set the id_info of the parameter as a side effect! *)
let params_of_parameters env params : scope =
  params |> Tok.unbracket
  |> List_.filter_map (function
       | Param { pname = Some id; pinfo = id_info; ptype = typ; _ } ->
           let sid = SId.of_tok ~file:env.file (snd id) in
           let resolved = { entname = (Parameter, sid); enttype = typ } in
           set_resolved env id_info resolved;
           Some (H.str_of_ident id, resolved)
       (* Destructuring parameter: the synthetic [parameter_classic]
        * carries a [!!_implicit_param!] binder that needs to be resolved
        * as a regular Parameter, so AST_to_IL can generate a
        * [pattern_assign_statements] prelude referencing it. The inner
        * pattern's leaves are declared as LocalVars when
        * [visit_function_definition] iterates [x.fparams] inside the
        * function scope and visits each pattern. *)
       | ParamPattern (_pat, { pname = Some id; pinfo = id_info; ptype = typ; _ }) ->
           let sid = SId.of_tok ~file:env.file (snd id) in
           let resolved = { entname = (Parameter, sid); enttype = typ } in
           set_resolved env id_info resolved;
           Some (H.str_of_ident id, resolved)
       (* Ruby [&callback] block parameter and PHP [&$var] by-reference
        * parameter are both produced as
        * [OtherParam("Ref", [Pa(Param(...))])] by their respective AST
        * converters. Treat the inner [Param] as a regular parameter so
        * references to it in the body resolve correctly. Scoped to these
        * two languages to avoid accidentally shadowing any other future
        * use of the [("Ref", _)] tag. *)
       | OtherParam
           ( ("Ref", _),
             [ Pa (Param { pname = Some id; pinfo = id_info; ptype = typ; _ })
             ] )
         when (match env.lang with
               | Lang.Ruby | Lang.Php -> true
               | _ -> false) ->
           let sid = SId.of_tok ~file:env.file (snd id) in
           let resolved = { entname = (Parameter, sid); enttype = typ } in
           set_resolved env id_info resolved;
           Some (H.str_of_ident id, resolved)
       | _ -> None)

let js_get_angular_constructor_args env attrs defs =
  let is_injectable =
    List.exists
      (function
        | NamedAttr (_, Id ((s, _), _), _) when is_js_angular_decorator s ->
            true
        | _ -> false)
      attrs
  in
  defs
  |> List_.filter_map (function
       | {
           s =
             DefStmt
               ( { name = EN (Id (("constructor", _), _)); _ },
                 FuncDef { fparams; _ } );
           _;
         }
         when is_injectable ->
           Some (params_of_parameters env fparams)
       | _ -> None)
  |> List_.flatten

let declare_var env lang id id_info ?(force_global=false) ?(is_macro=false)
    ~explicit vinit vtype =
  let sid = SId.of_tok ~file:env.file (snd id) in
  (* for the type, we use the (optional) type in vtype, or, if we can infer
   * the type of the expression vinit (literal or id), we use that as a type
   * useful when the type is not given, e.g. in Go: `var x = 2` *)
  let resolved_type = get_resolved_type lang (vinit, vtype) in
  let name_kind, add_ident_to_its_scope =
    (* In JS/TS an assignment to a variable that has not been
     * previously declared will implicitly create a property on
     * the *global* object. *)
    if Lang.is_js lang && not explicit ||
       Lang.is_c_cpp lang && is_macro || (* TODO: Clojure macro? *)
       force_global
    then
      (Global, add_ident_global_scope)
    else
      (resolved_name_kind env lang, add_ident_current_scope)
  in
  let resolved = { entname = (name_kind, sid); enttype = resolved_type } in
  add_ident_to_its_scope id resolved env.names;
  set_resolved env id_info resolved

let set_resolved_global_if_not_already_resolved env ?vinit id id_info =
  (* Used for all clojure non-auto-resolved atoms which we consider globals. *)
  if
    Option.is_none !(id_info.id_resolved)
  then
    match lookup_scope_opt id env with
    | Some resolved ->
      (* Name resolution. *)
      set_resolved env id_info resolved
    | _ ->
      (* Declare it once globally. *)
      declare_var env env.lang id id_info
        ~force_global:true
        ~explicit:false
        vinit None

let assign_implicitly_declares lang =
  lang =*= Lang.Php
  || lang =*= Lang.Python
  || lang =*= Lang.Ruby
  || lang =*= Lang.Crystal
  || Lang.is_js lang

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

class ['self] resolve_visitor env lang =
  object (self : 'self)
    inherit [_] AST_generic.iter_no_id_info as super

    val env = env
    val lang = lang

    (* ---------- *)
    (* !the defs! *)
    (* ---------- *)
    method! visit_function_definition venv x =
      (* todo: add the function as a Global. In fact we should do a first
       * pass for some languages to add all of them first, because
       * Go for example allow the use of forward function reference
       * (no need to declarare prototype and forward decls as in C).
       *)
      let new_params = params_of_parameters env x.fparams in
      (* A PHP function or closure body sees no enclosing local; a method
         body sees its class's scope, for the class constants; an arrow
         function sees everything. *)
      let hidden_blocks =
        let enclosing = List.length !(env.names.blocks) in
        match (lang, fst x.fkind) with
        | Lang.Php, (Function | LambdaKind | BlockCases) -> Some enclosing
        | Lang.Php, Method -> Some (max 0 (enclosing - 1))
        | Lang.Php, Arrow
        | _ ->
            !(env.hidden_blocks)
      in
      Common.save_excursion_unsafe env.hidden_blocks hidden_blocks (fun () ->
      with_new_context InFunction env (fun () ->
          with_new_function_scope new_params env.names (fun () ->
              (* Each [ParamPattern]'s synthetic implicit binder was just
               * registered as a Parameter in [new_params]. The inner
               * pattern's leaves still need to be declared in the
               * function scope so references in the body resolve: visit
               * each pattern in source order so the visitor's [PatId]
               * case calls [declare_var] for each leaf as a LocalVar.
               * AST_to_IL will later emit a [pattern_assign_statements]
               * prelude that binds these leaves to projections of the
               * implicit binder. *)
              (Tok.unbracket x.fparams)
              |> List.iter (function
                | ParamPattern (pat, _) -> super#visit_pattern venv pat
                | _ -> ());
              (* todo: actually we should first go inside x.fparams.ptype
               * without the new_params (this would also prevent cycle if
               * a parameter name is the same than type name used in ptype
               * (see tests/naming/python/shadow_name_type.py) *)
              super#visit_function_definition venv x)))

    method! visit_definition venv x =
      match x with
     | { attrs; _ } as entity, ClassDef c ->
          let class_params = params_of_parameters env c.cparams in
          with_new_context InClass env (fun () ->
              let special_class_params =
                if Lang.is_js lang then
                  let _, fields, _ = c.cbody in
                  js_get_angular_constructor_args env attrs
                    (List_.map (fun (F x) -> x) fields)
                else []
              in
              (* TODO? Maybe we need a `with_new_class_scope`. For now, abusing `with_new_function_scope`. *)
              with_new_function_scope (special_class_params @ class_params)
                env.names (fun () ->
                  self#visit_entity venv entity;
                  self#visit_class_definition venv (H.reorder_fields_in_class_definition c)))
      (* `const x = require('y');` (or var, or let)
       *
       * JS: This is a CommonJS import, popularized before ES6 standardized
       * imports/exports. *)
      | ( { name = EN (Id (id, id_info)); _ },
          VarDef
            {
              vinit =
                Some
                  {
                    e =
                      Call
                        ( { e = IdSpecial (Require, _); _ },
                          (_, [ Arg { e = L (String (_, file, _)); _ } ], _)
                        );
                    _;
                  };
              _;
            } )
        when lang =*= Lang.Js || lang =*= Lang.Ts ->
          let sid = SId.of_tok ~file:env.file (snd id) in
          let canonical = dotted_to_canonical [ file ] in
          let resolved = untyped_ent (ImportedModule canonical, sid) in
          set_resolved env id_info resolved;
          add_ident_current_scope id resolved env.names
      (* `const {x, y} = require('z');` (or var, or let)
       *
       * JS: This is a CommonJS import, popularized before ES6 standardized
       * imports/exports. *)
      | ( { name = EN (Id ((id_str, _), _)); _ },
          VarDef
            {
              vinit =
                Some
                  {
                    e =
                      Assign
                        ( { e = Record (_, fields, _); _ },
                          _,
                          {
                            e =
                              Call
                                ( { e = IdSpecial (Require, _); _ },
                                  ( _,
                                    [ Arg { e = L (String (_, file, _)); _ } ],
                                    _ ) );
                            _;
                          } );
                    _;
                  };
              _;
            } )
        when id_str = special_multivardef_pattern
             && (lang =*= Lang.Js || lang =*= Lang.Ts) ->
          List.iter
            (function
              | F
                  {
                    s =
                      DefStmt
                        ( {
                            name = EN (Id (imported_id, _imported_id_info));
                            attrs = [];
                            tparams = None;
                          },
                          FieldDefColon
                            {
                              vinit =
                                Some
                                  { e = N (Id (local_id, local_id_info)); _ };
                              _;
                            } );
                    _;
                  } ->
                  let sid = SId.of_tok ~file:env.file (snd local_id) in
                  let canonical = dotted_to_canonical [ file; imported_id ] in
                  let resolved =
                    untyped_ent (ImportedEntity canonical, sid)
                  in
                  set_resolved env local_id_info resolved;
                  add_ident_current_scope local_id resolved env.names
                  (* TODO handle nested destructuring? *)
                  (* TODO: Use the patterns mechanism to do this proper. *)
              | _ -> ())
            fields
      (* In Rust, the left-hand side (lhs) of the let variable definition is
       * parsed as a pattern.
       * TODO handle more cases than just the simple identifier pattern. *)
      | ( { name = EPattern (PatId (id, id_info)); attrs; tparams },
          VarDef { vinit; vtype; vtok = _ } )
      | { name = EN (Id (id, id_info)); attrs; tparams },
        VarDef { vinit; vtype; vtok = _ }
      (* note that some languages such as Python do not have VarDef
       * construct
       * todo? should add those somewhere instead of in_lvalue detection? *)
        when is_resolvable_name_ctx env lang ->
          (* The RHS resolves to existing bindings before declaring the
           * new variable. This is needed for variable shadowing like
           * `let x = x;` in Rust - the RHS x must refer to the outer
           * binding, not the newly declared one.
           * We cannot use super#visit_definition here because it would
           * visit the entity name (EPattern/EN), triggering visit_pattern
           * which declares the variable before vinit is visited.
           * See also the handling of LetPattern in visit_expr.
           *)
          Option.iter (self#visit_expr venv) vinit;
          (* Visit attrs and tparams before vtype, matching the original order
           * in the generic visitor. This matters for C++ template variables
           * where vtype may reference type parameters from tparams. *)
          List.iter (self#visit_attribute venv) attrs;
          Option.iter (self#visit_type_parameters venv) tparams;
          Option.iter (self#visit_type_ venv) vtype;
          declare_var env lang id id_info ~explicit:true vinit vtype
      (* Left the case above because we have the type information `vtype` which
       * would be lost here. *)
      | ( { name = EPattern (pat); _ }, VarDef { vinit = _; vtype = _; vtok = _ } )
        when is_resolvable_name_ctx env lang ->
          super#visit_definition venv x;
          self#visit_pattern venv pat
      | { name = EN (Id (id, id_info)); _ }, FuncDef _
        when is_resolvable_name_ctx env lang ->
          (* A function definition resolves to a positional sid via [of_tok] —
           * its identity is then [(name, file, line, col)], the same key
           * [Function_id] uses, so a call resolving to this name carries the
           * def's identity (interprocedural analysis).
           *
           * Scope: JS/TS resolve function names in any context (the
           * interprocedural feature those users requested, see
           *
           *     https://github.com/semgrep/semgrep/issues/2787).
           *
           * Other languages resolve only *top-level* function defs.  Resolving
           * class methods / nested functions regressed interprocedural taint —
           * a helper method sanitizing its argument stopped being recognized
           * (the Java XXE rules, which have a duplicated [setFeatures] helper).
           * Top-level functions are what name-based rules need, e.g.
           *
           *     semgrep-rules/python/flask/correctness/same-handler-name.yaml
           *
           * This rule tries to match two different functions using the same
           * meta-variable. This works when the function names are not
           * resolved, but breaks when each function gets a unique sid; hence
           * the flag set below.
           *
           * We add the name to the "imported" scope (not current scope):
           * current scope shadowed imported function names even when the
           * import came later, breaking
           *   semgrep-rules/python/django/security/audit/raw-query.py.
           * But do we need a special scope for imported functions? *)
          let resolve =
            match lang with
            | Lang.Js
            | Lang.Ts ->
                true
            | _ -> ( match top_context env with AtToplevel -> true | _ -> false)
          in
          if resolve then (
            let sid = SId.of_tok ~file:env.file (snd id) in
            let resolved = untyped_ent (resolved_name_kind env lang, sid) in
            add_ident_imported_scope id resolved env.names;
            set_resolved env id_info resolved;
            (* Mark the name as a function definition so the matcher can still
               unify two same-named defs — which now resolve to distinct
               positional sids — under a single metavar. *)
            id_info.id_flags := IdFlags.set_function_def !(id_info.id_flags));
          super#visit_definition venv x
      | { name = EN (Id (id, id_info)); _ }, UseOuterDecl tok ->
          (* PHP keywords are case-insensitive *)
          let s = String.lowercase_ascii (Tok.content_of_tok tok) in
          let flookup =
            match s with
            | "global" -> lookup_global_scope
            | "nonlocal" -> lookup_nonlocal_scope
            (* a PHP closure [use ($x)] *)
            | "use" -> lookup_enclosing_scope
            | _ ->
                error tok (spf "unrecognized UseOuterDecl directive: %s" s);
                lookup_global_scope
          in
          (match flookup id env.names with
          | Some resolved ->
              set_resolved env id_info resolved;
              add_ident_current_scope id resolved env.names
          | None when String.equal s "global" ->
              (* the directive creates the global when the file has not
                 assigned it yet, as the language does *)
              declare_var env lang id id_info ~force_global:true
                ~explicit:false None None;
              lookup_global_scope id env.names
              |> Option.iter (fun resolved ->
                     add_ident_current_scope id resolved env.names)
          | None ->
              error tok
                (spf "could not find '%s' for directive %s"
                   (H.str_of_ident id) s));
          super#visit_definition venv x
      (* module L = List, in OCaml *)
      | ( { name = EN (Id (id, id_info)); _ },
          ModuleDef { mbody = ModuleAlias xs } ) ->
          (* similar to the ImportAs case *)
          let sid = SId.of_tok ~file:env.file (snd id) in
          let canonical = dotted_to_canonical xs in
          let resolved = untyped_ent (ImportedModule canonical, sid) in
          set_resolved env id_info resolved;
          (* difference with ImportAs, we add in local scope in OCaml *)
          add_ident_current_scope id resolved env.names;
          super#visit_definition venv x
      | ( { name = EN (Id (id, id_info)); _ },
          MacroDef
            { macroparams = []; macrobody = [ E ({ e = L _; _ } as e) ] } ) ->
          declare_var env lang id id_info ~is_macro:true ~explicit:true (Some e) None;
          super#visit_definition venv x
      (* general case, just recurse *)
      | _ -> super#visit_definition venv x

    (* sgrep: the import aliases *)
    method! visit_directive venv x =
      (match x.d with
      | ImportFrom (_, DottedName xs, imported_names) ->
          List.iter
            (function
              | id, Some (alias, id_info) ->
                  (* for python *)
                  let sid = SId.of_tok ~file:env.file (snd alias) in
                  let canonical = dotted_to_canonical (xs @ [ id ]) in
                  let resolved =
                    untyped_ent (ImportedEntity canonical, sid)
                  in
                  set_resolved env id_info resolved;
                  add_ident_imported_scope alias resolved env.names
              | id, None ->
                  (* for python *)
                  let sid = SId.of_tok ~file:env.file (snd id) in
                  let canonical = dotted_to_canonical (xs @ [ id ]) in
                  let resolved =
                    untyped_ent (ImportedEntity canonical, sid)
                  in
                  add_ident_imported_scope id resolved env.names)
            imported_names
      | ImportFrom (_, FileName (s, tok), imported_names) ->
          List.iter
            (function
              | id, None
                when Lang.is_js lang && fst id <> Ast_js.default_entity ->
                  (* for JS we consider import { x } from 'a/b/foo' as foo.x.
                   * Note that we guard this code with is_js lang, because Python
                   * uses also Filename in 'from ...conf import x'.
                   *)
                  let sid = SId.of_tok ~file:env.file (snd id) in
                  let _, b, _ = Filename_.dbe_of_filename_noext_ok s in
                  let base = (b, tok) in
                  let canonical = dotted_to_canonical [ base; id ] in
                  let resolved =
                    untyped_ent (ImportedEntity canonical, sid)
                  in
                  add_ident_imported_scope id resolved env.names
              | id, Some (alias, id_info)
                when Lang.is_js lang && fst id <> Ast_js.default_entity ->
                  (* for JS *)
                  let sid = SId.of_tok ~file:env.file (snd alias) in
                  let _, b, _ = Filename_.dbe_of_filename_noext_ok s in
                  let base = (b, tok) in
                  let canonical = dotted_to_canonical [ base; id ] in
                  let resolved =
                    untyped_ent (ImportedEntity canonical, sid)
                  in
                  set_resolved env id_info resolved;
                  add_ident_imported_scope alias resolved env.names
              | _ -> ())
            imported_names
      | ImportAs (_, DottedName xs, Some (alias, id_info)) ->
          (* for python *)
          let sid = SId.of_tok ~file:env.file (snd alias) in
          let canonical = dotted_to_canonical xs in
          let resolved = untyped_ent (ImportedModule canonical, sid) in
          set_resolved env id_info resolved;
          add_ident_imported_scope alias resolved env.names
      | ImportAs (_, FileName (s, tok), Some (alias, id_info)) ->
          (* for Go *)
          let sid = SId.of_tok ~file:env.file (snd alias) in
          let pkgname = go_package_alias s in
          let base = (pkgname, tok) in
          let canonical = dotted_to_canonical [ base ] in
          let resolved = untyped_ent (ImportedModule canonical, sid) in
          set_resolved env id_info resolved;
          add_ident_imported_scope alias resolved env.names
      | _ -> ());
      super#visit_directive venv x

    method! visit_catch venv x =
      let _t, exn, _st = x in
      (match exn with
      (* TODO: we should create a new block scope *)
      | CatchParam { pname = Some id; pinfo = id_info; _ }
        when is_resolvable_name_ctx env lang ->
          declare_var env lang id id_info ~explicit:true None None
      | _ -> ());
      super#visit_catch venv x

    method! visit_pattern venv x =
      match x with
      | (PatId (id, id_info) | PatAs (_, (id, id_info)))
        when is_resolvable_name_ctx env lang ->
          (* todo: in Python it does not necessarily introduce
           * a newvar if the ID was already declared before.
           * Also inside a PatAs(PatId x,b), the 'x' is actually
           * the name of a class, not a newly introduced local.
           * NOTE (dimitris): I could not find any example where
           * such 'x' is not a new variable... why should it be
           * a class name?
           *)
          declare_var env lang id id_info ~explicit:true None None;
          (* TODO: Should we visit before declaring? Better, if we
           * want `[x, y] as x` to have `as x` shadowing the other `x`. *)
          super#visit_pattern venv x
      | PatTyped (PatId (id, id_info), ty)
        when is_resolvable_name_ctx env lang ->
          declare_var env lang id id_info ~explicit:true None (Some ty)
      | PatTyped (PatAs (pat', (id, id_info)), ty)
        (* TODO: Check if we need to visit `ty` also, first. *)
        when is_resolvable_name_ctx env lang ->
          super#visit_pattern venv pat';
          declare_var env lang id id_info ~explicit:true None (Some ty)
      (* do not recurse here, we don't want the PatId case above
       * to overwrite the job done here
       *)
      | PatTyped (pattern, ty) (* when Lang.is_js lang *) ->
        Common.save_excursion_unsafe env.in_lvalue true (fun () ->
            super#visit_type_ venv ty);
        super#visit_pattern venv pattern
      | OtherPat ((":", _),
                  [Name ((IdQualified {name_last = (id, _);
                                       name_middle = Some (QDots [_]);
                                       name_top = Some (Tok.FakeTok (":", _));
                                       name_info = id_info; _})
                         as name)]) 
        when lang =*= Lang.Clojure ->
        let vinit = (N name |> e |> Option.some) in 
        set_resolved_global_if_not_already_resolved env ?vinit id id_info
      (* This is used for Ts in the case of typed patterns with records.
       * For example in a fuction like: 
       *  function ({foo} :{foo:foo_type}){}
       * we need to make sure we do it safely for types otherwise they will
       * be put in the stack. see the test rules/ts_type
       *)
      | OtherPat _
      (* This interacts badly with implicit JS/TS declarations. It causes
       * `foo` in `function f({ foo }) { ... }` to be resolved as a global
       * variable, which in turn affects semgrep-rule _react-props-in-state_.
       * This when-clause achieves the previous behavior of leaving `foo`
       * unresolved. *)
      (* TODO: We should fix the AST of JS/TS so those `f({foo})` patterns do
       * not show as regular variables. *)
      (* TODO: Fix Js encoding to use proper patterns...
       * For example in:
       * `function f({ foo }) { sink(foo) }`
       * we should have a pattern PatKeyVal where key is PatId(foo) and value
       * is e_param.foo where e_param is the function parameter. *)
        when not (Lang.is_js lang) ->
          Common.save_excursion_unsafe env.in_lvalue true (fun () ->
              super#visit_pattern venv x)
      | _ -> super#visit_pattern venv x

    (* ---------- *)
    (* !the uses! *)
    (* ---------- *)
    (* kname will resolve names for Constructor/PatConstructor/NamedAttr/TyN
     * and maybe more. For expressions, we do something special for N (Id)
     * in kexpr do deal with languages where the first occurence of an
     * Id could be a declaration.
     *)
    method! visit_name venv x =
      match x with
      | Id (id, id_info) -> (
          if
            (* Avoid overwriting 'id_resolved'.
             * THINK: Maybe log something if we were going to overwrite ? *)
            Option.is_none !(id_info.id_resolved)
          then
            match lookup_scope_opt id env with
            | Some resolved ->
                (* name resolution *)
                set_resolved env id_info resolved
            | _ -> ())
      | IdQualified
          {
            name_last = id, None;
            name_middle;
            name_info = id_info;
            name_top = None;
          } ->
          (match name_middle with
          | Some (QDots ((m, None) :: rest_of_middle)) -> (
              match lookup_scope_opt m env with
              (* Resolve modules for OCaml *)
              | Some { entname = ImportedModule xs, _sidm; _ }
              (* Resolve classes for use in typed metavars (Java) *)
              (* Note that we only need to resolve the first name
                 because that is the only one that could be
                 imported *)
              | Some { entname = ImportedEntity xs, _sidm; _ } ->
                  (* Fully qualified — identity is the canonical name, not the
                     sid; still anchor the sid at the name's real place. *)
                  let sid = SId.of_tok ~file:env.file (snd id) in
                  let rest_of_middle = List_.map fst rest_of_middle in
                  let canonical =
                    xs @ dotted_to_canonical (rest_of_middle @ [ id ])
                  in
                  let resolved =
                    untyped_ent (ImportedEntity canonical, sid)
                  in
                  set_resolved env id_info resolved
              | _ -> ())
          | _ -> ());
          super#visit_name venv x
      | IdQualified _ -> ()

    method! visit_expr venv x =
      (* ugly: hack. If we use a classic recursive-with-env visitor,
       * we would not need this *)
      (* TODO now that we do use a classic recursive-with-env visitor,
       * refactor so this isn't needed. *)
      let recurse = ref true in
      (match x.e with
      (* Go: This is `x := E`, a single-variable short variable declaration.
       * When this declaration is legal, and that is when the same variable
       * has not yet been declared in the same scope, it *always* introduces
       * a new variable. (Quoting Go' spec, "redeclaration can only appear
       * in a multi-variable short declaration".)
       * See: https://golang.org/ref/spec#Short_variable_declarations *)
      | AssignOp ({ e = N (Id (id, id_info)); _ }, (Eq, tok), e2)
        when lang =*= Lang.Go
             && Tok.content_of_tok tok = ":="
             && is_resolvable_name_ctx env lang ->
          (* Need to visit the RHS first so that type is populated *)
          (* If we do var a = 3, then var b = a, we want to propagate the type of a *)
          super#visit_expr venv x;
          declare_var env lang id id_info ~explicit:true (Some e2) None;
          recurse := false
      | Assign ({ e = N (Id (id, id_info)); _ }, _, e2)
        when Option.is_none (lookup_for_implicit_assign_opt id env)
             && assign_implicitly_declares lang
             && is_resolvable_name_ctx env lang ->
          (* Need to visit the RHS first so that type is populated *)
          self#visit_expr venv e2;
          declare_var env lang id id_info ~explicit:false (Some e2) None;
          recurse := false
      (* todo: see lrvalue.ml
       * alternative? extra id_info tag?
       *)
      | Assign (e1, _, e2)
      | AssignOp (e1, _, e2) ->
          Common.save_excursion_unsafe env.in_lvalue true (fun () ->
              self#visit_expr venv e1);
          self#visit_expr venv e2;
          recurse := false
      | ArrayAccess (e1, (_, e2, _)) ->
          (* The base of a subscript is read even when the whole subscript
           * expression is the LHS of an assignment ([target[i] = v] reads
           * [target] to find the object to mutate). For languages with
           * implicit-declaration-on-assign (Python, Ruby, PHP, JS), this
           * matters: visiting [e1] with [in_lvalue := true] would treat
           * the base name as a write target and declare it locally,
           * shadowing the enclosing-scope binding. *)
          Common.save_excursion_unsafe env.in_lvalue false (fun () ->
              self#visit_expr venv e1;
              self#visit_expr venv e2);
          recurse := false
      (* specialized kname case when in expr context *)
      | N (Id (id, id_info)) ->
          (* A write target uses the same shadow-aware lookup as the
             single-name [Assign] case above: destructuring targets,
             augmented-assignment targets and (Ruby) top-level assignments
             all reach the name through here, and must declare a local
             rather than bind a same-named definition from an outer scope. *)
          let implicit_declaration =
            !(env.in_lvalue)
            && assign_implicitly_declares lang
            && is_resolvable_name_ctx env lang
          in
          let resolved =
            if implicit_declaration then lookup_for_implicit_assign_opt id env
            else lookup_scope_opt id env
          in
          (match resolved with
          | Some resolved ->
              (* name resolution *)
              set_resolved env id_info resolved
          | None ->
              if implicit_declaration then
                (* first use of a variable can be a VarDef in some languages *)
                declare_var env lang id id_info ~explicit:false None None
              else
                (* hopefully the lang-specific resolved may have resolved that *)
                (* TODO: this can happen because of in_lvalue bug detection, or
                 * for certain entities like functions or classes which are
                 * currently tagged
                 *)
                let s, tok = id in
                if is_implicit_param s then ()
                else
                  error tok (spf "could not find '%s' in environment" s));
          recurse := false
      | DotAccess
          ({ e = IdSpecial ((This | Self), _); _ }, _, FN (Id (id, id_info)))
        -> (
          match lookup_scope_opt ~class_attr:true id env with
          (* TODO: this is a v0 for doing naming and typing of fields.
           * we should really use a different lookup_scope_class, that
           * would handle shadowing of fields from locals, etc. but it's
           * a start.
           *)
          | Some ({ entname = EnclosedVar, _sid; _ } as resolved) ->
              set_resolved env id_info resolved;
              recurse := false
          | _ ->
              let s, tok = id in
              error tok (spf "could not find '%s' field in environment" s))
      | DotAccess (e1, _, fname) ->
          (* The receiver of a dot-access is read even when the whole
           * expression is the LHS of an assignment ([obj.field = v]
           * reads [obj] to find the object to mutate). Same reasoning
           * as ArrayAccess above. *)
          Common.save_excursion_unsafe env.in_lvalue false (fun () ->
              self#visit_expr venv e1);
          self#visit_field_name venv fname;
          recurse := false
      | Comprehension (_op, (_l, (e, xs), _r)) ->
          (* Actually in Python2, no new scope was created, so iterator vars
           * could leak in the outer scope. This was fixed in Python3. *)
          with_new_block_scope env.names (fun () ->
              (* first visit xs, then e *)
              xs |> List.iter (fun x -> self#visit_for_or_if_comp venv x);
              self#visit_expr venv e);
          recurse := false
      (* Because we want new names to be declared after e is visited, for
       * correct rebinding of the same variable in nested let. Therefore,
       * if x appears in e, it must be from the outer scope and won't be
       * the one declared by visiting pat first. *)
      | LetPattern (pat, e) ->
        self#visit_expr venv e;
        self#visit_pattern venv pat;
        recurse := false
      (* These expressions define scopes. *)
      (* TODO: Create module in clojure's directory, make this more
       * configurable (ask if construct is block etc.) *)
      | OtherExpr ((("ExprBlock" | "as->"), _block_tk),
                  expr_anys)
        when lang =*= Lang.Clojure ->
        (* Even if we parse a top level block, it does not
         * define global names. *)
        with_new_context InFunction env (fun () ->
          with_new_block_scope env.names (fun () ->
              List.iter (self#visit_any venv) expr_anys));
        recurse := false
      (* Clojure ShortLambda:
       * OtherExpr("ShortLambda", [Params [...]; E body])
       * Create a new scope with the params and visit the body. *)
      | OtherExpr (("ShortLambda", _),
                   [Params [(ParamPattern (pat, classic))]; E body])
        when lang =*= Lang.Clojure ->
        let new_params =
          params_of_parameters env
            (Tok.unsafe_fake_bracket [ ParamPattern (pat, classic) ])
        in
        with_new_context InFunction env (fun () ->
            with_new_function_scope new_params env.names (fun () ->
                self#visit_pattern venv pat;
                self#visit_expr venv body));
          recurse := false
      (* TODO: Without a condition on middle names, this identifies
       * :domain/a and :a. Needs more careful handling. *)
      | OtherExpr (("Atom", _atom_tk),
                   [Name ((IdQualified {name_last = (id, _);
                                        name_middle = Some (QDots [_]);
                                        name_top = Some (Tok.FakeTok (":", _));
                                        name_info = id_info; _})
                          as name)])
        when lang =*= Lang.Clojure ->
        let vinit = (N name |> e |> Option.some) in 
        set_resolved_global_if_not_already_resolved env ?vinit id id_info;
        recurse := false
      (* Elixir ShortLambda: OtherExpr("ShortLambda", [Params [...]; S body])
       * Create a new scope with the params and visit the body. *)
      | OtherExpr (("ShortLambda", _), [ Params params; S body ]) ->
          let new_params =
            params_of_parameters env (Tok.unsafe_fake_bracket params)
          in
          with_new_function_scope new_params env.names (fun () ->
              self#visit_stmt venv body);
          recurse := false
      | _ -> ());
      if !recurse then super#visit_expr venv x

    method! visit_type_ venv x =
      if !(env.in_type) then super#visit_type_ venv x
      else
        Common.save_excursion_unsafe env.in_type true (fun () ->
            super#visit_type_ venv x)

    (* TODO: support other types of statements that create block scopes. *)
    method! visit_stmt venv x =
      match x.s with
      | If (tok, Cond e, s1, s2_opt) when has_block_scope lang ->
          self#visit_tok venv tok;
          self#visit_expr venv e;
          with_new_block_scope env.names (fun () -> self#visit_stmt venv s1);
          Option.iter
            (fun s2 ->
              with_new_block_scope env.names (fun () ->
                  self#visit_stmt venv s2))
            s2_opt
      | If (tok, OtherCond (("LetCond", cond_tk), [P pat; E e]), s1, s2_opt)
        when has_block_scope lang ->
          self#visit_tok venv tok;
          self#visit_tok venv cond_tk;
          self#visit_expr venv e;
          with_new_block_scope env.names (fun () ->
              (* Identifiers introduced by the pattern are only local
               * to the if_branch. *)
              self#visit_pattern venv pat;
              self#visit_stmt venv s1);
          Option.iter
            (fun s2 ->
              with_new_block_scope env.names (fun () ->
                  self#visit_stmt venv s2))
            s2_opt
      (* But is there any point in doing that? Probably yes. *)
      (* Commented out: docker constant propagation test fails... *)
      (* | Block (_, stmts, _) when has_block_scope lang ->
             with_new_block_scope env.names (fun () ->
                 List.iter (fun stmt -> self#visit_stmt venv stmt) stmts) *)
      | _else_ -> super#visit_stmt venv x
  end
  
let resolve lang prog =
  (* The real file of the program, read off its first real token; used to
     stamp every resolved-name sid with its genuine source path. *)
  let file =
    match AST_generic_helpers.range_of_any_opt (Pr prog) with
    | Some (loc, _) -> Fpath.to_string (Fpath.normalize loc.Tok.pos.file)
    | None -> ""
  in
  let env = default_env lang file in

  (* coupling: we do similar things in Constant_propagation.ml so if you
   * add a feature here, you might want to add a similar thing over there too.
   *)
  let visitor = new resolve_visitor env lang
  in
  visitor#visit_program () prog;
  ()
[@@profiling]
