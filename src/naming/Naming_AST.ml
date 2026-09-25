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
  (* declared in a class body: a member of the class, static or not *)
  member : bool;
  (* a local the function scope declares when it opens, before the
     assignment that binds it is reached *)
  placeholder : bool;
}

type namespace =
  | VarName
  | FuncName
  | TypeNs

type scope_key = namespace * string

module SId_tbl = Hashtbl.Make (struct
  type t = SId.t

  let equal = SId.equal
  let hash = SId.hash
end)
type scope = (scope_key, scope_info) Assoc.t

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

let var_key (id : ident) : scope_key = (VarName, H.str_of_ident id)
let func_key (id : ident) : scope_key = (FuncName, H.str_of_ident id)
let type_key (id : ident) : scope_key = (TypeNs, H.str_of_ident id)

let equal_namespace (a : namespace) (b : namespace) : bool =
  match (a, b) with
  | VarName, VarName
  | FuncName, FuncName
  | TypeNs, TypeNs ->
      true
  | VarName, (FuncName | TypeNs)
  | FuncName, (VarName | TypeNs)
  | TypeNs, (VarName | FuncName) ->
      false

let add_key_current_scope (key : scope_key) resolved scopes =
  match !(scopes.blocks) with
  | [] -> scopes.global := (key, resolved) :: !(scopes.global)
  | xs :: xxs -> scopes.blocks := ((key, resolved) :: xs) :: xxs

let add_ident_current_scope id resolved scopes =
  add_key_current_scope (var_key id) resolved scopes

let add_func_ident_current_scope id resolved scopes =
  add_key_current_scope (func_key id) resolved scopes

(* for Python *)
let add_ident_imported_scope id resolved scopes =
  scopes.imported := (var_key id, resolved) :: !(scopes.imported)

let add_ident_global_scope id resolved scopes =
  scopes.global := (var_key id, resolved) :: !(scopes.global)

(* for JS 'var' *)
let _add_ident_function_scope _id _resolved _scopes = raise Todo
let untyped_ent name =
  { entname = name; enttype = None; member = false; placeholder = false }

let rec find_in_scope_where (ns : namespace) (s : string)
    (p : scope_info -> bool) (xs : scope) : scope_info option =
  match xs with
  | [] -> None
  | ((entry_ns, entry_s), res) :: xs ->
      if equal_namespace ns entry_ns && String.equal s entry_s && p res then
        Some res
      else find_in_scope_where ns s p xs

let find_in_scope (ns : namespace) (s : string) (xs : scope) :
    scope_info option =
  find_in_scope_where ns s (fun _ -> true) xs

(* [members]: whether the lookup may find a class member. *)
let rec lookup_namespace ?(members = true) ~(class_attr : bool) (ns : namespace)
    (s : string) (xxs : scope list) : scope_info option =
  let visible (res : scope_info) = members || not res.member in
  match xxs with
  | [] -> None
  | xs :: xxs -> (
      match find_in_scope_where ns s visible xs with
      | None -> lookup_namespace ~members ~class_attr ns s xxs
      | Some res when class_attr ->
          if res.member then Some res
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
          else lookup_namespace ~members ~class_attr ns s xxs
      | Some res -> Some res)

(* see also lookup_scope_opt below taking as a parameter the environment *)
let lookup ?(class_attr = false) s xxs =
  lookup_namespace ~class_attr VarName s xxs

let declared_in_current_block (scopes : scopes) (s : string) :
    scope_info option =
  match !(scopes.blocks) with
  | current :: _ -> find_in_scope VarName s current
  | [] -> None

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

let has_block_scope (lang : Lang.t) =
  match lang with
  (* These languages don't have block scope *)
  | Ruby
  | Crystal
  | Python
  | Python2
  | Python3
  | Php
  | Hack
  | Bash
  | R
  | Julia
  | Dockerfile
  | Clojure
  | Lisp
  | Scheme ->
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
  (* The real file being resolved.  A resolved name's sid carries its
     definition token's place within this file (see [AST_generic.SId]);
     naming processes a single file, so the token's file and this one
     coincide. *)
  file : string;
  (* The next binding number of this file; bindings are counted in
     traversal order, so two parses of the same bytes agree. *)
  next_binding : int ref;
  (* The declared field types of each record and class type of the file,
     by type name, then field name. *)
  field_types : (string, (string, type_) Hashtbl.t) Hashtbl.t;
  (* The class whose body is being visited, and the binding of its name. *)
  enclosing_class : (string * resolved_name) option ref;
  (* The comprehension scopes around the current point, inside the innermost
     function: an assignment expression in a comprehension binds in that
     function (Python). *)
  comprehension_depth : int ref;
  (* The scope of each class body, by the binding of the class's name: a
     qualified name [A::m] reaches the member [m] of [A] through it. *)
  class_scopes : scope SId_tbl.t;
  namespace : scope ref option ref;
  namespaces : (string * scope ref) list ref;
}

let fresh_binding (env : env) : int =
  let binding = !(env.next_binding) in
  env.next_binding := binding + 1;
  binding

let default_env lang file =
  {
    ctx = ref [ AtToplevel ];
    names = default_scopes ();
    hidden_blocks = ref None;
    in_lvalue = ref false;
    in_type = ref false;
    lang;
    file;
    next_binding = ref 1;
    field_types = Hashtbl.create 16;
    enclosing_class = ref None;
    comprehension_depth = ref 0;
    class_scopes = SId_tbl.create 16;
    namespace = ref None;
    namespaces = ref [];
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

let in_class env =
  match top_context env with
  | InClass -> true
  | AtToplevel
  | InFunction ->
      false

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

let split_blocks (env : env) : scope list * scope list =
  let blocks = !(env.names.blocks) in
  match !(env.hidden_blocks) with
  | None -> (blocks, [])
  | Some hidden ->
      let visible = List.length blocks - hidden in
      ( List.filteri (fun (i : int) (_ : scope) -> i < visible) blocks,
        List.filteri (fun (i : int) (_ : scope) -> i >= visible) blocks )

(* the block scopes a lookup may see, innermost first *)
let visible_blocks env = fst (split_blocks env)

(* Behind a function body's gate (see [hidden_blocks]), the names the body
   still sees, searched in the hidden enclosing scopes first, then in the file
   scope: PHP's functions and constants (no [$] sigil), since a function
   declared inside a function is global once declared; Ruby's globals ([$]
   sigil) and constants (capitalised), which are visible everywhere. *)
let visible_behind_gate (lang : Lang.t) (s : string) : bool =
  match lang with
  | Lang.Php -> not (String.starts_with ~prefix:"$" s)
  | Lang.Ruby
  | Lang.Crystal -> (
      String.starts_with ~prefix:"$" s
      ||
      match s.[0] with
      | 'A' .. 'Z' -> true
      | _
      | (exception Invalid_argument _) ->
          false)
  | _ -> true

(* A class's members are in scope, without [this], in its methods. *)
let members_in_scope_in_methods (lang : Lang.t) : bool =
  match lang with
  (* true for Java so that we can type class fields *)
  | Lang.Java
  | Lang.Kotlin
  | Lang.Apex
  | Lang.Csharp
  | Lang.Vb
  | Lang.Scala
  | Lang.Dart
  | Lang.Swift
  | Lang.Solidity
  | Lang.C (* can happen for macros inside structs *)
  | Lang.Cpp ->
      true
  | _ -> false

(* The members a class declares are resolved. In JS, TS and PHP a method
 * reaches them only through [this]. *)
let members_resolved (lang : Lang.t) : bool =
  members_in_scope_in_methods lang
  ||
  match lang with
  (* true for JS/TS so that we can resolve class methods *)
  | Lang.Js
  | Lang.Ts
  | Lang.Php ->
      true
  | _ -> false

(* accessors *)

(* The scopes a lookup of the name [s] sees, innermost first. *)
let scopes_seen ~(in_lvalue : bool) (ns : namespace) (s : string) (env : env) :
    scope list =
  let scopes = env.names in
  let with_namespace (file : scope list) : scope list =
    match (!(env.namespace), ns) with
    | None, _ -> file
    | Some current, TypeNs -> [ !current ]
    | Some current, (VarName | FuncName) -> !current :: file
  in
  match !(scopes.blocks) with
    | [] -> with_namespace [ !(scopes.global); !(scopes.imported) ]
    | xs :: xxs -> (
        match env.lang with
        | Lang.Python ->
            if
              in_lvalue
              (* just look current scope! no access to nested scopes or global *)
            then [ xs; !(scopes.imported) ]
            else [ xs ] @ xxs @ [ !(scopes.global); !(scopes.imported) ]
        | Lang.Php
        | Lang.Ruby
        | Lang.Crystal ->
            (* just look current scope! no access to nested scopes or global:
             * a function body sees its own locals, what a [global] directive
             * or a closure [use] planted in it, and the names
             * [visible_behind_gate] lets through, searched in the hidden
             * enclosing scopes first, then in the file scope (a PHP function
             * declared inside a function is global once declared; Ruby
             * constants and globals are visible everywhere), not the file's
             * variables; an arrow function sees the scopes enclosing it, the
             * file's variables included when no function body is in
             * between *)
            (* A Ruby [def] body is such a gate too; its blocks are not. *)
            let visible, gated = split_blocks env in
            let enclosing, file_scope =
              if
                Option.is_none !(env.hidden_blocks)
                || visible_behind_gate env.lang s
              then (gated, [ !(scopes.global) ])
              else ([], [])
            in
            visible @ enclosing
            @ with_namespace (file_scope @ [ !(scopes.imported) ])
        | _ ->
            [ xs ] @ xxs
            @ with_namespace [ !(scopes.global); !(scopes.imported) ])

let lookup_namespace_opt ~(class_attr : bool) (ns : namespace) ((s, _) : ident)
    (env : env) : scope_info option =
  (* without [this], a member is reachable only where the language says so *)
  let members = class_attr || members_in_scope_in_methods env.lang in
  lookup_namespace ~members ~class_attr ns s
    (scopes_seen ~in_lvalue:!(env.in_lvalue) ns s env)

let lookup_scope_opt ?(class_attr = false) id env =
  lookup_namespace_opt ~class_attr VarName id env

let lookup_func_scope_opt (id : ident) (env : env) : scope_info option =
  lookup_namespace_opt ~class_attr:false FuncName id env

(* A class's nested types are in scope, without qualification, in the class
   body and, where the language says so, in its methods; Ruby and Crystal
   constants are lexically scoped. *)
let nested_types_in_scope_in_methods (lang : Lang.t) : bool =
  members_in_scope_in_methods lang
  ||
  match lang with
  | Lang.Ruby
  | Lang.Crystal ->
      true
  | _ -> false

let types_are_values (lang : Lang.t) : bool =
  match lang with
  | Lang.Python
  | Lang.Python2
  | Lang.Python3
  | Lang.Js
  | Lang.Lua
  | Lang.Julia
  | Lang.Clojure ->
      true
  | _ -> false

let type_namespace (lang : Lang.t) : namespace =
  if types_are_values lang then VarName else TypeNs

let lookup_type_scope_opt ((s, _) : ident) (env : env) : scope_info option =
  let members = nested_types_in_scope_in_methods env.lang || in_class env in
  let ns = type_namespace env.lang in
  lookup_namespace ~members ~class_attr:false ns s
    (scopes_seen ~in_lvalue:false ns s env)

(* The bare name of a field or method identifies a member of the receiver,
 * never a binding in scope, so it takes no identity from a same-named binding. It
 * still takes its type: a struct field declaration is recorded in the file
 * scope as a typed global, a constructor parameter 'http: Ty' types the
 * field 'this.http' (see tests/rules/js_constructor_naming), and a typed
 * metavariable reads the type off the bare name. *)
let type_field_from_scope env id id_info =
  match lookup_scope_opt id env with
  | Some { enttype = Some { t = TyFun _; _ }; _ }
  | Some { enttype = None; _ }
  | None ->
      ()
  | Some { enttype = Some ty; _ } ->
      if Option.is_none !(id_info.id_type) && not !(env.in_type) then
        id_info.id_type := Some ty

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
 * Hack: an assignment in a function body declares a local of that
 * function, as in PHP; a lambda captures the enclosing variables by
 * value, so an assignment in it declares a local of the lambda. Only the
 * current block scope suppresses the declaration.
 *
 * R: [<-] assigns in the environment of the function it runs in, whatever
 * an enclosing function binds, so only the current block scope suppresses
 * the declaration.
 *
 * Ruby / Crystal: blocks and procs close over enclosing locals, so an
 * assignment anywhere on the block chain rebinds them; top-level locals
 * live in the global scope and stay visible (script-style code). What
 * assignment does shadow is a same-named top-level [def]: defs live in
 * the imported scope, which is excluded here.
 *
 * Julia: inside a function an assignment rebinds a local of that function
 * or of an enclosing one, or a name a [global] directive binds; any other
 * name becomes a local of the function. At the top level it rebinds a
 * global of the file.
 *
 * JS, Lua and Bash keep the full-chain lookup: a JS bare assignment
 * genuinely mutates the outer binding, and a Lua or Bash assignment
 * rebinds the innermost visible local, else the global of the name.
 *)
let lookup_for_implicit_assign_opt id env =
  let s, _ = id in
  match (env.lang, !(env.names.blocks)) with
  | (Lang.Python | Lang.Python2 | Lang.Python3), (_ :: _ as blocks) ->
      (* the function's own scope, seen through the comprehensions inside it *)
      lookup s
        (List.filteri
           (fun (i : int) (_ : scope) -> i <= !(env.comprehension_depth))
           blocks)
  | (Lang.Python | Lang.Python2 | Lang.Python3), [] ->
      (* at the top level an assignment rebinds a variable of the file; a
         name bound by an import or a definition gets a new binding here *)
      lookup s [ !(env.names.global) ]
  | (Lang.Php | Lang.Hack | Lang.R), current_block :: _ ->
      lookup s [ current_block ]
  | (Lang.Ruby | Lang.Crystal), _ ->
      (* Blocks close over enclosing locals, and top-level locals stay
         visible; [blocks] is empty at the top level, where the chain is
         just the global scope.  The imported scope — where top-level
         [def]s live — is excluded either way: that is what assignment
         shadows (locals and methods are separate namespaces). *)
      let file_scope =
        if
          Option.is_none !(env.hidden_blocks)
          || visible_behind_gate env.lang s
        then [ !(env.names.global) ]
        else []
      in
      lookup s (visible_blocks env @ file_scope)
  | Lang.Julia, (_ :: _ as blocks) -> lookup s blocks
  | Lang.Julia, [] -> lookup s [ !(env.names.global) ]
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
  | InClass -> members_resolved lang

(* A constructor is written with the name of its class, and that name denotes
   the class. *)
let constructor_named_after_class (lang : Lang.t) : bool =
  match lang with
  | Lang.Java
  | Lang.Apex
  | Lang.Csharp
  | Lang.Cpp
  | Lang.Dart ->
      true
  | _ -> false

let has_function_namespace (lang : Lang.t) : bool =
  match lang with
  | Lang.Java
  | Lang.Kotlin ->
      true
  | _ -> false

let resolved_name_kind env lang =
  match top_context env with
  | AtToplevel -> Global
  | InFunction -> LocalVar
  | InClass ->
      (* alt: use a different scope.class? *)
      if members_resolved lang then EnclosedVar else raise Impossible

(* !also set the id_info of the parameter as a side effect! *)
let params_of_parameters env params : scope =
  params |> Tok.unbracket
  |> List_.filter_map (function
       | Param { pname = Some id; pinfo = id_info; ptype = typ; _ }
       | ParamReceiver { pname = Some id; pinfo = id_info; ptype = typ; _ }
       | ParamRest (_, { pname = Some id; pinfo = id_info; ptype = typ; _ })
       | ParamHashSplat (_, { pname = Some id; pinfo = id_info; ptype = typ; _ })
         ->
           let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd id) in
           let resolved =
             { entname = (Parameter, sid); enttype = typ; member = false;
               placeholder = false }
           in
           set_resolved env id_info resolved;
           Some (var_key id, resolved)
       (* Destructuring parameter: the synthetic [parameter_classic]
        * carries a [!!_implicit_param!] binder that needs to be resolved
        * as a regular Parameter, so AST_to_IL can generate a
        * [pattern_assign_statements] prelude referencing it. The inner
        * pattern's leaves are declared as LocalVars when
        * [visit_function_definition] iterates [x.fparams] inside the
        * function scope and visits each pattern. *)
       | ParamPattern (_pat, { pname = Some id; pinfo = id_info; ptype = typ; _ }) ->
           let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd id) in
           let resolved =
             { entname = (Parameter, sid); enttype = typ; member = false;
               placeholder = false }
           in
           set_resolved env id_info resolved;
           Some (var_key id, resolved)
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
           let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd id) in
           let resolved =
             { entname = (Parameter, sid); enttype = typ; member = false;
               placeholder = false }
           in
           set_resolved env id_info resolved;
           Some (var_key id, resolved)
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

(* The scope a definition made here binds its name in. *)
let current_scope (env : env) : scope =
  match !(env.names.blocks) with
  | [] -> !(env.names.global)
  | current :: _ -> current

(* The entry that [scope] already holds for the definition of [id] at this
   very site: a hoist, or a class member declared before its body is
   visited. *)
let same_site_entry (env : env) (ns : namespace) (id : ident) (scope : scope)
    : scope_info option =
  let site = SId.of_site ~file:env.file (snd id) in
  find_in_scope_where ns (H.str_of_ident id)
    (fun ({ entname = _, sid; _ } : scope_info) -> SId.same_site sid site)
    scope

let current_scope_entry (env : env) (ns : namespace) (id : ident) :
    scope_info option =
  same_site_entry env ns id (current_scope env)

let implicit_declaration_is_global (lang : Lang.t) : bool =
  match lang with
  | Lang.Lua
  | Lang.Bash ->
      true
  | _ -> Lang.is_js lang

let add_to_namespace (ns : scope ref) (key : scope_key) (resolved : scope_info)
    : unit =
  ns := (key, resolved) :: !ns

let add_ident_namespace (ns : scope ref) (id : ident) (resolved : scope_info)
    (_ : scopes) : unit =
  add_to_namespace ns (var_key id) resolved

(* Every name a function body binds is a local of the whole body, whatever
   binds it and wherever in the body. *)
let locals_are_function_scoped (lang : Lang.t) : bool =
  match lang with
  | Lang.Python
  | Lang.Python2
  | Lang.Python3
  | Lang.Php
  | Lang.Hack ->
      true
  | _ -> false

let bindings_in_textual_order (lang : Lang.t) : bool =
  match lang with
  | Lang.Python
  | Lang.Python2
  | Lang.Python3
  | Lang.Lua ->
      true
  | _ -> false

let rebinds_in_order (env : env) : bool =
  bindings_in_textual_order env.lang
  &&
  match !(env.names.blocks) with
  | [] -> true
  | _ :: _ ->
      locals_are_function_scoped env.lang
      && Int.equal !(env.comprehension_depth) 0

let file_scope_in_order (env : env) : bool =
  bindings_in_textual_order env.lang && List.is_empty !(env.names.blocks)

let binding_scopes (env : env) : scope list =
  match !(env.names.blocks) with
  | [] -> [ !(env.names.global); !(env.names.imported) ]
  | current :: _ -> [ current ]

let enclosing_scopes (env : env) : scope list =
  match !(env.names.blocks) with
  | [] -> []
  | _ :: outer -> outer @ [ !(env.names.global); !(env.names.imported) ]

let assignment_binds_here (env : env) (id : ident) : bool =
  rebinds_in_order env
  &&
  let s = H.str_of_ident id in
  match lookup s (binding_scopes env) with
  | None -> true
  | Some { entname = _, bound; _ } -> (
      match lookup s (enclosing_scopes env) with
      | Some { entname = _, outer; _ } -> not (SId.equal outer bound)
      | None -> true)

let file_namespace (env : env) : scope ref option =
  match !(env.names.blocks) with
  | [] -> !(env.namespace)
  | _ :: _ -> None

let declare_var env lang id id_info ?(force_global=false) ?(is_macro=false)
    ?(static = false) ?(constant = false) ~explicit vinit vtype =
  (* In JS/TS an assignment to a variable that has not been
   * previously declared will implicitly create a property on
   * the *global* object. *)
  let global =
    implicit_declaration_is_global lang && not explicit ||
    Lang.is_c_cpp lang && is_macro || (* TODO: Clojure macro? *)
    force_global
  in
  let namespace = if constant then file_namespace env else None in
  let rebound =
    if (not explicit) && assignment_binds_here env id then
      match lookup (H.str_of_ident id) (binding_scopes env) with
      | Some { entname = (ImportedEntity _ | ImportedModule _), _; _ }
      | None ->
          None
      | Some entry -> Some entry
    else None
  in
  let sid =
    match current_scope_entry env VarName id with
    | Some { entname = _, sid; _ } -> sid
    | None -> (
        match rebound with
        | None -> (
            (* the local the scope declared when it opened, see
               [predeclare_locals] *)
            match
              find_in_scope_where VarName (H.str_of_ident id)
                (fun (entry : scope_info) -> entry.placeholder)
                (match (global, namespace) with
                | true, _ -> !(env.names.global)
                | false, Some ns -> !ns
                | false, None -> current_scope env)
            with
            | Some { entname = _, sid; _ } -> sid
            | None ->
                SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd id))
        | Some { entname = _, bound; _ } ->
            SId.of_tok ~binding:(SId.to_int bound) ~file:env.file (snd id))
  in
  (* for the type, we use the (optional) type in vtype, or, if we can infer
   * the type of the expression vinit (literal or id), we use that as a type
   * useful when the type is not given, e.g. in Go: `var x = 2` *)
  let resolved_type = get_resolved_type lang (vinit, vtype) in
  let name_kind, add_ident_to_its_scope =
    if global then
      (Global, add_ident_global_scope)
    else
      match (namespace, top_context env, static) with
      | Some ns, _, _ -> (Global, add_ident_namespace ns)
      (* A static member is one variable of its class, not of each object:
       * a global, visible where the class's members are. *)
      | None, InClass, true -> (Global, add_ident_current_scope)
      | None, _, _ -> (resolved_name_kind env lang, add_ident_current_scope)
  in
  let name_kind =
    match rebound with
    | Some { entname = TypeName, _; _ }
    | None ->
        name_kind
    | Some { entname = kind, _; _ } -> kind
  in
  let resolved =
    {
      entname = (name_kind, sid);
      enttype = resolved_type;
      member = in_class env;
      placeholder = false;
    }
  in
  add_ident_to_its_scope id resolved env.names;
  set_resolved env id_info resolved

let declare_func env lang (id : ident) id_info (frettype : type_ option) =
  let resolved =
    match current_scope_entry env FuncName id with
    | Some resolved -> resolved
    | None ->
        let binding =
          match find_in_scope FuncName (H.str_of_ident id) (current_scope env) with
          | Some { entname = _, overloaded; _ } -> SId.to_int overloaded
          | None -> fresh_binding env
        in
        let entname =
          ( resolved_name_kind env lang,
            SId.of_tok ~binding ~file:env.file (snd id) )
        in
        let resolved =
          { entname; enttype = frettype; member = in_class env;
            placeholder = false }
        in
        add_func_ident_current_scope id resolved env.names;
        resolved
  in
  set_resolved env id_info resolved

let bind_definition (env : env) (kind : resolved_name_kind) ~(member : bool)
    ~(keeps_type : bool) (id : ident) : scope_info =
    (* The scope a definition binds its name in: at the top level, the
       current PHP or Hack namespace, else the file's imported scope (see
       the [FuncDef] case of the visitor); elsewhere the enclosing block, a
       function's for a nested function, a class's for a method. A
       definition of a name that scope already binds rebinds it: the same
       identity, at its own site. *)
    let scope, add_to_scope =
      match (file_namespace env, !(env.names.blocks)) with
      | Some ns, _ -> (!ns, add_ident_namespace ns)
      | None, [] -> (!(env.names.imported), add_ident_imported_scope)
      | None, current :: _ -> (current, add_ident_current_scope)
    in
    match same_site_entry env VarName id scope with
    | Some resolved ->
        (* The definition is reached after its hoist: from here on a use
           sees this definition, the latest one of the name. *)
        add_to_scope id resolved env.names;
        if file_scope_in_order env then
          add_ident_global_scope id resolved env.names;
        resolved
    | None ->
        (* A rebinding keeps the type the scope had for the name: a Java
           field and its same-named accessor are one entry here, and
           the field's type is what a use of the name carries. An import
           is another file's definition, never rebound here. *)
        let binding, enttype =
          match
            find_in_scope_where VarName (fst id)
              (fun ({ entname = kind, _; _ } : scope_info) ->
                match kind with
                | ImportedEntity _
                | ImportedModule _ ->
                    false
                | _ -> true)
              scope
          with
          | Some { entname = _, bound; enttype; _ } ->
              (SId.to_int bound, if keeps_type then enttype else None)
          | None -> (fresh_binding env, None)
        in
        let sid = SId.of_tok ~binding ~file:env.file (snd id) in
        let resolved =
          { entname = (kind, sid); enttype; member; placeholder = false }
        in
        add_to_scope id resolved env.names;
        resolved

(* The binding a definition of a value (a function, a module) gives its name
   in the value namespace. *)
let bind_value_definition env lang (id : ident) (id_info : id_info) : unit =
  set_resolved env id_info
    (bind_definition env (resolved_name_kind env lang) ~member:(in_class env)
       ~keeps_type:true id)

(* The binding a function definition gives its name. *)
(* A method declared with a receiver (Go [func (t T) M()], a Rust method
   taking [self]) belongs to its receiver type's methods and binds nothing in
   the enclosing scope. *)
let declared_with_receiver (fdef : function_definition) : bool =
  match Tok.unbracket fdef.fparams with
  | ParamReceiver _ :: _ -> true
  | _ -> false

let bind_function_definition env lang (id : ident) (id_info : id_info)
    (fdef : function_definition) : unit =
  let frettype = fdef.frettype in
  match !(env.enclosing_class) with
  | _ when declared_with_receiver fdef && not (in_class env) -> ()
  | Some (class_name, class_binding)
    when in_class env
         && constructor_named_after_class lang
         && String.equal (H.str_of_ident id) class_name ->
      id_info.id_resolved := Some class_binding
  | Some _
  | None ->
      if has_function_namespace lang then
        declare_func env lang id id_info frettype
      else bind_value_definition env lang id id_info

(* Types are declared at the top level, in modules and in classes, not in
 * function bodies, so only those are walked. *)
let rec collect_field_types (env : env) (stmts : stmt list) : unit =
  let record (tname : string) (fields : field list) =
    let types = Hashtbl.create 8 in
    fields
    |> List.iter (fun (F stmt) ->
           match stmt.s with
           | DefStmt
               ({ name = EN (Id ((fname, _), _)); _ }, VarDef { vtype = Some ty; _ })
             ->
               Hashtbl.replace types fname ty
           | _ -> ());
    Hashtbl.replace env.field_types tname types;
    collect_field_types env (List_.map (fun (F stmt) -> stmt) fields)
  in
  stmts
  |> List.iter (fun (stmt : stmt) ->
         match stmt.s with
         | DefStmt
             ( { name = EN (Id ((tname, _), _)); _ },
               TypeDef
                 { tbody = NewType { t = TyRecordAnon (_, (_, fields, _)); _ } }
             )
         | DefStmt
             ({ name = EN (Id ((tname, _), _)); _ }, ClassDef { cbody = _, fields, _; _ })
           ->
             record tname fields
         | DefStmt (_, ModuleDef { mbody = ModuleStruct (_, items) }) ->
             collect_field_types env items
         | Block (_, stmts, _) -> collect_field_types env stmts
         | _ -> ())

(* The declared type of data field [fname] of the type [receiver_type] refers
 * to, seen through pointers. *)
let rec field_type (env : env) (receiver_type : type_) (fname : string) :
    type_ option =
  match receiver_type.t with
  | TyPointer (_, t)
  | TyRef (_, t) ->
      field_type env t fname
  | TyN (Id ((tname, _), _)) ->
      Option.bind (Hashtbl.find_opt env.field_types tname) (fun types ->
          Hashtbl.find_opt types fname)
  | _ -> None

let declare_class_members env lang (c : class_definition) : unit =
  if is_resolvable_name_ctx env lang then
    let _, fields, _ = c.cbody in
    fields
    |> List.iter (fun (F stmt) ->
           match stmt.s with
           | DefStmt
               ( { name = EN (Id (id, id_info)); attrs; _ },
                 VarDef { vinit; vtype; vtok = _ } ) ->
               declare_var env lang id id_info
                 ~static:(H.has_keyword_attr Static attrs)
                 ~explicit:true vinit vtype
           | DefStmt
               ({ name = EN (Id (id, id_info)); _ }, FuncDef fdef)
             when has_function_namespace lang ->
               bind_function_definition env lang id id_info fdef
           | _ -> ())

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

let assign_implicitly_declares (lang : Lang.t) : bool =
  match lang with
  | Lang.Php
  | Lang.Hack
  | Lang.Python
  | Lang.Python2
  | Lang.Python3
  | Lang.Ruby
  | Lang.Crystal
  | Lang.Lua
  | Lang.R
  | Lang.Bash
  | Lang.Julia ->
      true
  | _ -> Lang.is_js lang

(* The member [id] of the class the path [first :: rest] leads to: [first] is a
   class in scope, each of [rest] a type nested in the previous one. In a type
   the member is a nested type; elsewhere it is a field or a method, and only
   where fields and methods have separate namespaces (Java, Kotlin) can both
   exist, the field being what a name outside a call reads. *)
let member_of_class_path (env : env) (first : ident)
    (rest : (ident * type_arguments option) list) (id : ident) :
    scope_info option =
  let class_scope (resolved : scope_info) : scope option =
    match resolved.entname with
    | TypeName, sid -> SId_tbl.find_opt env.class_scopes sid
    | _ -> None
  in
  let rec walk (scope : scope) (path : (ident * type_arguments option) list) =
    match path with
    | [] ->
        let s = H.str_of_ident id in
        let namespaces =
          if !(env.in_type) then [ type_namespace env.lang ]
          else [ VarName; FuncName ]
        in
        List.find_map (fun (ns : namespace) -> find_in_scope ns s scope)
          namespaces
    | (nested, _) :: path ->
        Option.bind
          (Option.bind
             (find_in_scope (type_namespace env.lang) (H.str_of_ident nested)
                scope)
             class_scope)
          (fun scope -> walk scope path)
  in
  Option.bind
    (Option.bind (lookup_type_scope_opt first env) class_scope)
    (fun scope -> walk scope rest)

(* The binding a type definition gives its name. Where types are values, it
   binds in the value namespace as a function does, a redefinition rebinding
   the name at its own site; elsewhere it binds in the type namespace of the
   current scope (at file level in a PHP or Hack namespace, of that
   namespace), taking the binding a hoist or an earlier definition of the
   name there made, else a new one.
   A type binding carries no type, so a type name never gets an [id_type]. *)
let declare_type (env : env) ~(member : bool) (id : ident) : scope_info =
  if types_are_values env.lang then
    bind_definition env TypeName ~member ~keeps_type:false id
  else
    let scope, add_to_scope =
      match file_namespace env with
      | Some ns -> (!ns, add_to_namespace ns)
      | None ->
          ( current_scope env,
            fun (key : scope_key) (resolved : scope_info) ->
              add_key_current_scope key resolved env.names )
    in
    match find_in_scope TypeNs (H.str_of_ident id) scope with
    | Some resolved -> resolved
    | None ->
        let sid =
          SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd id)
        in
        let resolved =
          { entname = (TypeName, sid); enttype = None; member; placeholder = false }
        in
        add_to_scope (type_key id) resolved;
        resolved

let bind_type_definition (env : env) (id : ident) (id_info : id_info) : unit =
  id_info.id_resolved := Some (declare_type env ~member:(in_class env) id).entname

let rec binding_of_type (ty : type_) : resolved_name option =
  match ty.t with
  | TyN (Id (_, info))
  | TyN (IdQualified { name_info = info; _ }) ->
      !(info.id_resolved)
  | TyApply (ty, _) -> binding_of_type ty
  | _ -> None

(* The type parameters of a generic definition are types in a scope of their
   own, around the definition's body. *)
let with_type_parameters (env : env) (tparams : type_parameters option)
    (f : unit -> unit) : unit =
  match tparams with
  | None
  | Some (_, [], _) ->
      f ()
  | Some (_, tps, _) ->
      with_new_block_scope env.names (fun () ->
          tps
          |> List.iter (function
               | TP { tp_id; _ } -> ignore (declare_type env ~member:false tp_id)
               | TParamEllipsis _
               | OtherTypeParam _ ->
                   ());
          f ())

(* A [var] declaration binds for the whole function, or for the file at the
   top level. *)
let hoisted_var_declarations (lang : Lang.t) : bool = Lang.is_js lang

let free_names_are_globals (lang : Lang.t) : bool =
  match lang with
  | Lang.Lua -> true
  | _ -> false

let rec lvalue_leaves (e : expr) : ident list =
  match e.e with
  | N (Id (id, _)) -> [ id ]
  | Container ((Tuple | List | Array), (_, es, _)) ->
      List.concat_map lvalue_leaves es
  | Call ({ e = IdSpecial (Spread, _); _ }, (_, [ Arg e ], _))
  | Cast (_, _, e) ->
      lvalue_leaves e
  | _ -> []

let rec pattern_leaves (p : pattern) : ident list =
  match p with
  | PatId (id, _) -> [ id ]
  | PatAs (p, (id, _)) -> id :: pattern_leaves p
  | PatTyped (p, _)
  | PatWhen (p, _) ->
      pattern_leaves p
  | PatTuple (_, ps, _)
  | PatList (_, ps, _)
  | PatConstructor (_, ps) ->
      List.concat_map pattern_leaves ps
  | PatRecord (_, fields, _) ->
      List.concat_map (fun (_, p) -> pattern_leaves p) fields
  | PatKeyVal (p1, p2)
  | PatDisj (p1, p2) ->
      pattern_leaves p1 @ pattern_leaves p2
  | PatLiteral _
  | PatWildcard _
  | PatType _
  | PatEllipsis _
  | DisjPat _
  | OtherPat _ ->
      []

(* The names [stmts] bind as locals of the scope they open, in source
   order, and the names they exclude from it ([global], [nonlocal], class
   names, which are types). A nested function or class binds its own,
   except in Lua, where an assignment target at any depth that no local
   declaration in the file binds is a global of the file. *)
let locals_bound_in (lang : Lang.t) (stmts : stmt list) :
    ident list * string list =
  let bound = ref [] and excluded = ref [] in
  let add (ids : ident list) = bound := List.rev_append ids !bound in
  let function_scoped = locals_are_function_scoped lang in
  let globals = free_names_are_globals lang in
  let exclude (ids : ident list) =
    excluded := List.rev_append (List.map H.str_of_ident ids) !excluded
  in
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_function_definition () (fdef : function_definition) =
        if globals then (
          Tok.unbracket fdef.fparams
          |> List.iter (function
               | Param { pname = Some id; _ }
               | ParamRest (_, { pname = Some id; _ }) ->
                   exclude [ id ]
               | _ -> ());
          super#visit_function_definition () fdef)

      method! visit_class_definition () (_ : class_definition) = ()

      method! visit_definition () ((ent, def) as definition) =
        (match (ent.name, def) with
        | EN (Id (id, _)), VarDef _
          when function_scoped || H.has_keyword_attr Var ent.attrs ->
            add [ id ]
        | EPattern pat, VarDef _
          when function_scoped || H.has_keyword_attr Var ent.attrs ->
            add (pattern_leaves pat)
        | EN (Id (id, _)), UseOuterDecl _
        | EN (Id (id, _)), ClassDef _ ->
            exclude [ id ]
        | EN (Id (id, _)), (VarDef _ | FuncDef _) when globals -> exclude [ id ]
        | EPattern pat, VarDef _ when globals -> exclude (pattern_leaves pat)
        | _ -> ());
        super#visit_definition () definition

      method! visit_expr () (e : expr) =
        if function_scoped || globals then
          match e.e with
          | Assign (lhs, _, _)
          | AssignOp (lhs, _, _) ->
              add (lvalue_leaves lhs);
              super#visit_expr () e
          | LetPattern (pat, _) ->
              add (pattern_leaves pat);
              super#visit_expr () e
          | _ -> super#visit_expr () e

      method! visit_stmt () (st : stmt) =
        (if function_scoped then
           match st.s with
           | For (_, ForEach (pat, _, _), _) -> add (pattern_leaves pat)
           | OtherStmt (OS_Delete, anys) ->
               anys
               |> List.iter (function
                    | E e -> add (lvalue_leaves e)
                    | _ -> ())
           | DirectiveStmt { d = ImportFrom (_, _, names); _ } ->
               names
               |> List.iter (fun (id, alias) ->
                      match alias with
                      | Some (alias, _) -> add [ alias ]
                      | None -> add [ id ])
           | DirectiveStmt { d = ImportAs (_, _, Some (alias, _)); _ } ->
               add [ alias ]
           | _ -> ());
        (if globals then
           match st.s with
           | For (_, ForEach (pat, _, _), _) -> exclude (pattern_leaves pat)
           | For (_, ForClassic (inits, _, _), _) ->
               inits
               |> List.iter (function
                    | ForInitVar ({ name = EN (Id (id, _)); _ }, _) ->
                        exclude [ id ]
                    | _ -> ())
           | _ -> ());
        super#visit_stmt () st

      method! visit_case () (c : case) =
        (if function_scoped then
           match c with
           | Case (_, pat) -> add (pattern_leaves pat)
           | _ -> ());
        super#visit_case () c

      method! visit_catch () ((_, exn, _) as c : catch) =
        (if function_scoped then
           match exn with
           | CatchParam { pname = Some id; _ } -> add [ id ]
           | CatchPattern pat -> add (pattern_leaves pat)
           | _ -> ());
        super#visit_catch () c
    end
  in
  List.iter (visitor#visit_stmt ()) stmts;
  (List.rev !bound, !excluded)

(* The locals a scope binds are declared when the scope opens, as
   placeholders the binding occurrence then takes over. In Lua the file
   scope also declares, when it opens, the globals assigned anywhere in the
   file. *)
let predeclare_locals (env : env) (stmts : stmt list) : unit =
  if
    locals_are_function_scoped env.lang
    || hoisted_var_declarations env.lang
    || (free_names_are_globals env.lang && List.is_empty !(env.names.blocks))
  then
    let bound, excluded = locals_bound_in env.lang stmts in
    bound
    |> List.iter (fun (id : ident) ->
           let s = H.str_of_ident id in
           if
             (not (List.exists (String.equal s) excluded))
             && Option.is_none (find_in_scope VarName s (current_scope env))
           then
             let sid =
               SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd id)
             in
             add_key_current_scope (var_key id)
               {
                 entname = (resolved_name_kind env env.lang, sid);
                 enttype = None;
                 member = false;
                 placeholder = true;
               }
               env.names)

let namespaces_hold_definitions (lang : Lang.t) : bool =
  match lang with
  | Lang.Php
  | Lang.Hack ->
      true
  | _ -> false

let enter_namespace (env : env) (directive : directive_kind) : unit =
  if namespaces_hold_definitions env.lang then
    match directive with
    | Package (_, []) | PackageEnd _ -> env.namespace := None
    | Package (_, path) ->
        let name = String.concat "\\" (List_.map H.str_of_ident path) in
        env.namespace :=
          Some
            (match
               List.find_opt
                 (fun ((known, _) : string * scope ref) -> String.equal known name)
                 !(env.namespaces)
             with
            | Some (_, ns) -> ns
            | None ->
                let ns = ref [] in
                env.namespaces := (name, ns) :: !(env.namespaces);
                ns)
    | _ -> ()

(* The definitions a scope makes are bound when the scope opens, so that a
   use before a definition sees it. A nested scope (a function, class or
   module body, a block with a scope of its own) binds its own definitions
   when it opens. *)
let hoist_definitions (env : env) (stmts : stmt list) : unit =
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_directive () (d : directive) = enter_namespace env d.d

      method! visit_definition () ((ent, def) : definition) =
        match (ent.name, def) with
        | EN (Id (id, _)), (ClassDef _ | TypeDef _) ->
            ignore (declare_type env ~member:(in_class env) id)
        | EN (Id (id, id_info)), FuncDef fdef
          when is_resolvable_name_ctx env env.lang ->
            bind_function_definition env env.lang id id_info fdef
        | EN (Id (id, id_info)), ModuleDef { mbody = ModuleStruct _ }
          when is_resolvable_name_ctx env env.lang ->
            bind_value_definition env env.lang id id_info
        | _ -> ()

      method! visit_expr () (_ : expr) = ()

      method! visit_stmt () (stmt : stmt) =
        match stmt.s with
        | Block _ when has_block_scope env.lang -> ()
        | _ -> super#visit_stmt () stmt
    end
  in
  List.iter (visitor#visit_stmt ()) stmts

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
        | (Lang.Ruby | Lang.Crystal), Method -> Some enclosing
        | Lang.Php, Arrow
        | _ ->
            !(env.hidden_blocks)
      in
      (* A capture refers to a variable where the closure is created; an
         initialised capture is a variable of the closure, whose initial
         value is computed there. *)
      let captures =
        x.fcaptures.clist
        |> List_.map (fun (c : capture) ->
               match c.cinit with
               | None -> (c, lookup_scope_opt (fst c.cname) env)
               | Some init ->
                   self#visit_expr venv init;
                   (c, None))
      in
      let x = { x with fcaptures = no_captures } in
      Common.save_excursion_unsafe env.comprehension_depth 0 (fun () ->
      Common.save_excursion_unsafe env.hidden_blocks hidden_blocks (fun () ->
      with_new_context InFunction env (fun () ->
          with_new_function_scope new_params env.names (fun () ->
              captures
              |> List.iter (fun ((c : capture), resolved) ->
                     let id, id_info = c.cname in
                     match (c.cinit, resolved) with
                     | None, Some resolved ->
                         set_resolved env id_info resolved;
                         add_ident_current_scope id resolved env.names
                     | None, None -> ()
                     | Some _, _ ->
                         declare_var env lang id id_info ~explicit:true None
                           None);
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
              (* The parameters and the body's top level are one scope. *)
              match x.fbody with
              | FBStmt { s = Block (_, stmts, _); _ } ->
                  hoist_definitions env stmts;
                  predeclare_locals env stmts;
                  super#visit_function_definition venv
                    { x with fbody = FBNothing };
                  List.iter (self#visit_stmt venv) stmts
              | _ -> super#visit_function_definition venv x))))

    method! visit_definition venv x =
      match x with
     | { attrs; _ } as entity, ClassDef c ->
          let enclosing_class =
            match entity.name with
            | EN (Id (id, id_info)) ->
                bind_type_definition env id id_info;
                Option.map
                  (fun (binding : resolved_name) -> (H.str_of_ident id, binding))
                  !(id_info.id_resolved)
            | _ -> None
          in
          Common.save_excursion_unsafe env.enclosing_class enclosing_class (fun () ->
          with_type_parameters env entity.tparams (fun () ->
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
                  let _, fields, _ = c.cbody in
                  declare_class_members env lang c;
                  hoist_definitions env (List_.map (fun (F stmt) -> stmt) fields);
                  self#visit_entity venv entity;
                  self#visit_class_definition venv c;
                  match (enclosing_class, !(env.names.blocks)) with
                  | Some (_, (_, class_sid)), body :: _ ->
                      SId_tbl.replace env.class_scopes class_sid body
                  | _ -> ()))))
      | ( { name = EN (Id (id, id_info)); tparams; _ }, TypeDef _ ) ->
          bind_type_definition env id id_info;
          with_type_parameters env tparams (fun () ->
              super#visit_definition venv x)
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
          let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd id) in
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
                  let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd local_id) in
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
          declare_var env lang id id_info
            ~static:(H.has_keyword_attr Static attrs)
            ~constant:(H.has_keyword_attr Const attrs)
            ~explicit:true vinit vtype
      (* Left the case above because we have the type information `vtype` which
       * would be lost here. *)
      | ( { name = EPattern (pat); _ }, VarDef { vinit = _; vtype = _; vtok = _ } )
        when is_resolvable_name_ctx env lang ->
          super#visit_definition venv x;
          self#visit_pattern venv pat
      | { name = EN (Id (id, id_info)); tparams; _ }, FuncDef fdef
        when is_resolvable_name_ctx env lang ->
          (* A function definition resolves to a sid that carries the def's
           * site, [(name, file, line, col)], the key [Function_id] uses, so
           * a call resolving to this name reaches the def's signature
           * (interprocedural analysis).
           *
           * Scope: every function, method and nested function definition
           * resolves, in every language (the interprocedural feature JS/TS
           * users requested first, see
           *
           *     https://github.com/semgrep/semgrep/issues/2787).
           *
           * Resolving class methods once regressed interprocedural taint —
           * a helper method sanitizing its argument stopped being recognized
           * (the Java XXE rules, which have a duplicated [setFeatures]
           * helper); those rules guard this now.
           * Top-level functions are what name-based rules need, e.g.
           *
           *     semgrep-rules/python/flask/correctness/same-handler-name.yaml
           *
           * This rule tries to match two different functions using the same
           * meta-variable. This works when the function names are not
           * resolved, and breaks when each function gets a unique sid; two
           * definitions of one name in one scope share their binding.
           *
           * We add the name to the "imported" scope (not current scope):
           * current scope shadowed imported function names even when the
           * import came later, breaking
           *   semgrep-rules/python/django/security/audit/raw-query.py.
           * But do we need a special scope for imported functions? *)
          bind_function_definition env lang id id_info fdef;
          with_type_parameters env tparams (fun () ->
              super#visit_definition venv x)
      | { name = EN (Id (id, id_info)); _ }, UseOuterDecl tok ->
          (* PHP keywords are case-insensitive *)
          let s = String.lowercase_ascii (Tok.content_of_tok tok) in
          let flookup =
            match s with
            | "global" -> lookup_global_scope
            | "nonlocal" -> lookup_nonlocal_scope
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
      | entity, ModuleDef { mbody = ModuleStruct (_, items) } ->
          (match entity.name with
          | EN (Id (id, id_info)) when is_resolvable_name_ctx env lang ->
              bind_value_definition env lang id id_info
          | _ -> ());
          self#visit_entity venv entity;
          with_new_block_scope env.names (fun () ->
              hoist_definitions env items;
              List.iter (self#visit_stmt venv) items)
      (* module L = List, in OCaml *)
      | ( { name = EN (Id (id, id_info)); _ },
          ModuleDef { mbody = ModuleAlias xs } ) ->
          (* similar to the ImportAs case *)
          let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd id) in
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
      | entity, OtherDef (("Impl", tok), T self_ty :: rest) ->
          self#visit_entity venv entity;
          self#visit_type_ venv self_ty;
          with_new_block_scope env.names (fun () ->
              binding_of_type self_ty
              |> Option.iter (fun (resolved : resolved_name) ->
                     add_key_current_scope
                       (type_key ("Self", tok))
                       (untyped_ent resolved) env.names);
              List.iter (self#visit_any venv) rest)
      (* general case, just recurse *)
      | _ -> super#visit_definition venv x

    (* The name a definition gives is a binder, bound by the definition's own
       case above where the language binds it, and never a use: a lookup
       would give it an unrelated binding of the same name. A qualified
       definition name keeps its qualifier, which is a use. *)
    method! visit_entity venv (ent : entity) =
      match ent.name with
      | EN (Id _) ->
          List.iter (self#visit_attribute venv) ent.attrs;
          Option.iter (self#visit_type_parameters venv) ent.tparams
      | EN (IdQualified _)
      | EDynamic _
      | EPattern _
      | OtherEntity _ ->
          super#visit_entity venv ent

    (* sgrep: the import aliases *)
    method! visit_directive venv x =
      (* An import in a function binds in the function; elsewhere, in the
         file's imported scope. *)
      let add_import id resolved scopes =
        match top_context env with
        | InFunction -> add_ident_current_scope id resolved scopes
        | AtToplevel
        | InClass ->
            add_ident_imported_scope id resolved scopes;
            if file_scope_in_order env then
              add_ident_global_scope id resolved scopes
      in
      enter_namespace env x.d;
      (match x.d with
      | ImportFrom (_, DottedName xs, imported_names) ->
          List.iter
            (function
              | id, Some (alias, id_info) ->
                  (* for python *)
                  let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd alias) in
                  let canonical = dotted_to_canonical (xs @ [ id ]) in
                  let resolved =
                    untyped_ent (ImportedEntity canonical, sid)
                  in
                  set_resolved env id_info resolved;
                  add_import alias resolved env.names
              | id, None ->
                  (* for python *)
                  let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd id) in
                  let canonical = dotted_to_canonical (xs @ [ id ]) in
                  let resolved =
                    untyped_ent (ImportedEntity canonical, sid)
                  in
                  add_import id resolved env.names)
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
                  let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd id) in
                  let _, b, _ = Filename_.dbe_of_filename_noext_ok s in
                  let base = (b, tok) in
                  let canonical = dotted_to_canonical [ base; id ] in
                  let resolved =
                    untyped_ent (ImportedEntity canonical, sid)
                  in
                  add_import id resolved env.names
              | id, Some (alias, id_info)
                when Lang.is_js lang && fst id <> Ast_js.default_entity ->
                  (* for JS *)
                  let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd alias) in
                  let _, b, _ = Filename_.dbe_of_filename_noext_ok s in
                  let base = (b, tok) in
                  let canonical = dotted_to_canonical [ base; id ] in
                  let resolved =
                    untyped_ent (ImportedEntity canonical, sid)
                  in
                  set_resolved env id_info resolved;
                  add_import alias resolved env.names
              | _ -> ())
            imported_names
      | ImportAs (_, DottedName xs, Some (alias, id_info)) ->
          (* for python *)
          let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd alias) in
          let canonical = dotted_to_canonical xs in
          let resolved = untyped_ent (ImportedModule canonical, sid) in
          set_resolved env id_info resolved;
          add_import alias resolved env.names
      | ImportAs (_, FileName (s, tok), Some (alias, id_info)) ->
          (* for Go *)
          let sid = SId.of_tok ~binding:(fresh_binding env) ~file:env.file (snd alias) in
          let pkgname = go_package_alias s in
          let base = (pkgname, tok) in
          let canonical = dotted_to_canonical [ base ] in
          let resolved = untyped_ent (ImportedModule canonical, sid) in
          set_resolved env id_info resolved;
          add_import alias resolved env.names
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
            (* In a type a name denotes a type; a language whose types are
               also values (a Python class is a variable of its module)
               binds some of them only as values, so a type position falls
               back to the value namespace. Elsewhere a name denotes a
               value; a class used as a value (called, or passed) has its
               binding in the type namespace. *)
            let first, second =
              let lookup_value (id : ident) (env : env) = lookup_scope_opt id env in
              if !(env.in_type) then (lookup_type_scope_opt, lookup_value)
              else (lookup_value, lookup_type_scope_opt)
            in
            match first id env with
            | Some resolved -> set_resolved env id_info resolved
            | None -> (
                match second id env with
                | Some resolved -> set_resolved env id_info resolved
                | None -> ()))
      | IdQualified
          { name_last = id, targs; name_middle = None; name_top = None; name_info }
        ->
          (match targs with
          | None when has_function_namespace lang && not !(env.in_type) ->
              (* A callable reference [::f] denotes a property, a function or
                 a class. A property is a variable, looked up first as any
                 name in value position is; a function has a namespace of
                 its own, which a variable of the same name shadows; a class
                 comes last, as for a class used as a value. *)
              if Option.is_none !(name_info.id_resolved) then
                [ (fun (id : ident) (env : env) -> lookup_scope_opt id env);
                  lookup_func_scope_opt;
                  lookup_type_scope_opt ]
                |> List.find_map (fun lookup -> lookup id env)
                |> Option.iter (set_resolved env name_info)
          | _ -> self#visit_name venv (Id (id, name_info)));
          super#visit_name venv x
      | IdQualified
          { name_last = id, _; name_middle = None; name_top = Some _; name_info }
        ->
          Common.save_excursion_unsafe env.names.blocks [] (fun () ->
              Common.save_excursion_unsafe env.namespace None (fun () ->
                  self#visit_name venv (Id (id, name_info))));
          super#visit_name venv x
      | IdQualified
          {
            name_last = id, _;
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
                     sid: every use of the name shares binding 0, which is
                     never minted, so a metavariable unifies over them; the
                     sid is still anchored at the name's real place. *)
                  let sid = SId.of_tok ~binding:0 ~file:env.file (snd id) in
                  let rest_of_middle = List_.map fst rest_of_middle in
                  let canonical =
                    xs @ dotted_to_canonical (rest_of_middle @ [ id ])
                  in
                  let resolved =
                    untyped_ent (ImportedEntity canonical, sid)
                  in
                  set_resolved env id_info resolved
              | _ -> (
                  (* a member of a class this file defines *)
                  match member_of_class_path env m rest_of_middle id with
                  | Some resolved -> set_resolved env id_info resolved
                  | None -> ()))
          | _ -> ());
          super#visit_name venv x
      | IdQualified _ -> super#visit_name venv x

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
      | AssignOp ({ e = Container (Tuple, (_, lhs, _)); _ }, (Eq, tok), e2)
        when lang =*= Lang.Go
             && Tok.content_of_tok tok = ":="
             && is_resolvable_name_ctx env lang ->
          self#visit_expr venv e2;
          lhs
          |> List.iter (fun (lhs_e : expr) ->
                 match lhs_e.e with
                 | N (Id (((s, _) as id), id_info))
                   when (not (String.equal s "_"))
                        && Option.is_none
                             (declared_in_current_block env.names s) ->
                     declare_var env lang id id_info ~explicit:true None None
                 | _ ->
                     Common.save_excursion_unsafe env.in_lvalue true (fun () ->
                         self#visit_expr venv lhs_e));
          recurse := false
      | Assign
          ( ({
               e =
                 ( N (Id (id, id_info))
                 | Cast (_, _, { e = N (Id (id, id_info)); _ }) );
               _;
             } as lhs),
            _,
            e2 )
        when (match lookup_for_implicit_assign_opt id env with
             | None -> true
             | Some (entry : scope_info) ->
                 entry.placeholder || assignment_binds_here env id)
             && assign_implicitly_declares lang
             && is_resolvable_name_ctx env lang ->
          (* Need to visit the RHS first so that type is populated *)
          self#visit_expr venv e2;
          let vtype =
            match lhs.e with
            | Cast (ty, _, _) ->
                self#visit_type_ venv ty;
                Some ty
            | _ -> None
          in
          declare_var env lang id id_info ~explicit:false (Some e2) vtype;
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
      | Call
          ( {
              e =
                N
                  (( Id (id, id_info)
                   | IdQualified
                       {
                         name_last = id, _;
                         name_middle = None;
                         name_top = None;
                         name_info = id_info;
                       } ) as name);
              _;
            },
            args )
        when has_function_namespace lang -> (
          match lookup_func_scope_opt id env with
          | Some resolved ->
              set_resolved env id_info resolved;
              super#visit_name venv name;
              self#visit_arguments venv args;
              recurse := false
          | None -> ())
      (* specialized kname case when in expr context *)
      | N (Id (id, id_info)) ->
          (* A write target uses the same shadow-aware lookup as the
             single-name [Assign] case above: destructuring targets,
             augmented-assignment targets and (Ruby) top-level assignments
             all reach the name through here, and must declare a local
             rather than bind a same-named definition from an outer scope. *)
          let implicit_declaration =
            !(env.in_lvalue)
            && (not !(env.in_type))
            && assign_implicitly_declares lang
            && is_resolvable_name_ctx env lang
          in
          let resolved =
            if implicit_declaration then lookup_for_implicit_assign_opt id env
            else if !(env.in_type) then
              match lookup_type_scope_opt id env with
              | Some _ as resolved -> resolved
              | None -> lookup_scope_opt id env
            else
              match lookup_scope_opt id env with
              | Some _ as resolved -> resolved
              | None -> lookup_type_scope_opt id env
          in
          (match resolved with
          | Some _ when implicit_declaration && assignment_binds_here env id ->
              declare_var env lang id id_info ~explicit:false None None
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
          | Some ({ member = true; _ } as resolved) ->
              set_resolved env id_info resolved;
              recurse := false
          | _ ->
              let s, tok = id in
              error tok (spf "could not find '%s' field in environment" s);
              type_field_from_scope env id id_info;
              recurse := false)
      | DotAccess (e1, _, fname) ->
          (* The receiver of a dot-access is read even when the whole
           * expression is the LHS of an assignment ([obj.field = v]
           * reads [obj] to find the object to mutate). Same reasoning
           * as ArrayAccess above. *)
          Common.save_excursion_unsafe env.in_lvalue false (fun () ->
              self#visit_expr venv e1);
          (* The bare name of a field or method identifies a member of the
           * receiver, never a binding in scope. A member that shares the name
           * of a function in scope is not a reference to that function, so
           * this code sets no [id_resolved] on the bare name; the project
           * index resolves a method bare name by receiver type. The bare name
           * gets the type the receiver's type declares for the field, which
           * a typed metavariable reads. *)
          (match fname with
           | FN (Id ((s, _), id_info)) -> (
               let receiver_type =
                 match e1.e with
                 | N (Id (_, info))
                 | DotAccess (_, _, FN (Id (_, info))) ->
                     !(info.id_type)
                 | _ -> None
               in
               match
                 Option.bind receiver_type (fun ty -> field_type env ty s)
               with
               | Some ty ->
                   id_info.id_flags := IdFlags.set_data_field !(id_info.id_flags);
                   (* On an offset a function type marks a method. *)
                   (match ty.t with
                   | TyFun _ -> ()
                   | _ ->
                       if Option.is_none !(id_info.id_type) && not !(env.in_type)
                       then id_info.id_type := Some ty)
               | None -> ())
           | FN (IdQualified _)
           | FDynamic _ ->
               self#visit_field_name venv fname);
          recurse := false
      | Comprehension (_op, (_l, (e, xs), _r)) ->
          (* Actually in Python2, no new scope was created, so iterator vars
           * could leak in the outer scope. This was fixed in Python3. *)
          Common.save_excursion_unsafe env.comprehension_depth
            (!(env.comprehension_depth) + 1) (fun () ->
          with_new_block_scope env.names (fun () ->
              (* first visit xs, then e *)
              xs |> List.iter (fun x -> self#visit_for_or_if_comp venv x);
              self#visit_expr venv e));
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
      let visit () =
        match x.t with
        (* The fields of a record type are members, as in a [ClassDef]. *)
        | TyRecordAnon _ ->
            with_new_context InClass env (fun () ->
                with_new_block_scope env.names (fun () ->
                    super#visit_type_ venv x))
        | _ -> super#visit_type_ venv x
      in
      if !(env.in_type) then visit ()
      else Common.save_excursion_unsafe env.in_type true visit

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
      (* Kotlin: the condition of do-while sees the body's declarations. *)
      | DoWhile (tok, body, cond) when lang =*= Lang.Kotlin ->
          self#visit_tok venv tok;
          with_new_block_scope env.names (fun () ->
              (match body.s with
              | Block (_, stmts, _) ->
                  hoist_definitions env stmts;
                  List.iter (self#visit_stmt venv) stmts
              | _ -> self#visit_stmt venv body);
              self#visit_expr venv cond)
      | WithUsingResource (tok, resources, body) when has_block_scope lang ->
          self#visit_tok venv tok;
          with_new_block_scope env.names (fun () ->
              List.iter (self#visit_stmt venv) resources;
              self#visit_stmt venv body)
      (* In these languages the whole switch body is one scope. Converters
       * group a case's statements in a block with fake brackets; a case
       * written with braces keeps its own scope. *)
      | Switch (tok, cond_opt, cases)
        when lang =*= Lang.C || lang =*= Lang.Cpp || lang =*= Lang.Java
             || lang =*= Lang.Csharp ->
          self#visit_tok venv tok;
          Option.iter (self#visit_condition venv) cond_opt;
          with_new_block_scope env.names (fun () ->
              cases
              |> List.iter (function
                   | CasesAndBody (case_list, body) -> (
                       List.iter (self#visit_case venv) case_list;
                       match body.s with
                       | Block ((l, stmts, _) : stmt list bracket)
                         when Tok.is_fake l ->
                           hoist_definitions env stmts;
                           List.iter (self#visit_stmt venv) stmts
                       | _ -> self#visit_stmt venv body)
                   | CaseEllipsis _ as case_and_body ->
                       self#visit_case_and_body venv case_and_body))
      (* But is there any point in doing that? Probably yes. *)
      (* Commented out: docker constant propagation test fails... *)
      | Block (_, stmts, _) when has_block_scope lang ->
          with_new_block_scope env.names (fun () ->
              hoist_definitions env stmts;
              List.iter (fun stmt -> self#visit_stmt venv stmt) stmts)
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
  collect_field_types env prog;
  hoist_definitions env prog;
  env.namespace := None;
  if hoisted_var_declarations lang || free_names_are_globals lang then
    predeclare_locals env prog;
  visitor#visit_program () prog;
  ()
[@@profiling]
