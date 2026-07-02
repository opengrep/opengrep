(* Single-pass AST walker for projidx: each file is walked twice total —
   [Visit_function_defs.fold_with_parent_path] for [Func_def] (keeps the
   parent-path tracking fdef consumers need), a plain [G.iter_no_id_info]
   visitor for the class-like defs.  The forward-order [Observation.t list]
   is cached on [file_info.fi_observations]; consumers filter on the
   variant they need instead of re-walking the AST. *)

module G = AST_generic

module Observation = struct
  type t =
    (* [opt_ent] is [None] for anonymous funcs; [parent_path] is the
       lexical scope path, outermost first ([Visit_function_defs]
       convention). *)
    | Func_def of {
        opt_ent : G.entity option;
        parent_path : IL.name option list;
        fdef : G.function_definition;
      }
    (* Every [G.ClassDef], including nested classes. *)
    | Class_def of {
        ent : G.entity;
        cdef : G.class_definition;
      }
    (* Go [type T struct/interface], TS [type X = ...], etc. *)
    | Type_def of {
        ent : G.entity;
        tdef : G.type_definition;
      }
    (* Language-extension def kinds; [kind] is the parser's tag
       (e.g. ["Impl"] for Rust). *)
    | Other_def of {
        ent : G.entity;
        kind : string;
        anys : G.any list;
      }
    (* Plain [VarDef]s; a [VarDef] whose [vinit] is a lambda comes
       through [Func_def] instead ([Visit_function_defs] semantics). *)
    | Var_def of {
        ent : G.entity;
        vdef : G.variable_definition;
      }
end

let walk_class_like_defs (ast : G.program) : Observation.t list =
  let acc = ref [] in
  let v = object
    inherit [_] G.iter_no_id_info as super
    method! visit_definition () ((ent, def_kind) as def) =
      (match def_kind with
       | G.ClassDef cdef ->
         acc := Observation.Class_def { ent; cdef } :: !acc
       | G.TypeDef tdef ->
         acc := Observation.Type_def { ent; tdef } :: !acc
       | G.OtherDef ((kind, _), anys) ->
         acc := Observation.Other_def { ent; kind; anys } :: !acc
       | G.VarDef vdef ->
         acc := Observation.Var_def { ent; vdef } :: !acc
       | _ -> ());
      super#visit_definition () def
  end in
  v#visit_program () ast;
  List.rev !acc

(* [iter_no_id_info] (rather than plain [iter]) skips [id_svalue]'s
   [Sym expr] refs; otherwise the walker follows svalue propagation
   into expressions outside the scope and pollutes per-fdef analyses. *)
let make_collecting_visitor
    ~(skip_nested_fdefs : bool)
    ?(on_expr : (G.expr -> unit) = fun _ -> ())
    ?(on_stmt : (G.stmt -> unit) = fun _ -> ())
    () =
  object
    inherit [_] G.iter_no_id_info as super
    method! visit_expr () e = on_expr e; super#visit_expr () e
    method! visit_stmt () s = on_stmt s; super#visit_stmt () s
    method! visit_function_definition () fdef =
      if skip_nested_fdefs then ()
      else super#visit_function_definition () fdef
  end

(* [~skip_nested_fdefs:true] suppresses descent into inner function
   bodies — for analyses that must attribute exprs to the enclosing
   fdef only. *)
let fold_exprs_in_stmt ?(skip_nested_fdefs = false)
    (f : 'acc -> G.expr -> 'acc) (init : 'acc) (s : G.stmt) : 'acc =
  let acc = ref init in
  let v =
    make_collecting_visitor ~skip_nested_fdefs
      ~on_expr:(fun e -> acc := f !acc e) ()
  in
  v#visit_stmt () s;
  !acc

(* Like [fold_exprs_in_stmt] but folds over sub-statements. *)
let fold_stmts_in_stmt ?(skip_nested_fdefs = false)
    (f : 'acc -> G.stmt -> 'acc) (init : 'acc) (s : G.stmt) : 'acc =
  let acc = ref init in
  let v =
    make_collecting_visitor ~skip_nested_fdefs
      ~on_stmt:(fun s' -> acc := f !acc s') ()
  in
  v#visit_stmt () s;
  !acc

let fold_stmts_in_program ?skip_nested_fdefs
    (f : 'acc -> G.stmt -> 'acc) (init : 'acc) (prog : G.program) : 'acc =
  List.fold_left (fun acc top ->
    fold_stmts_in_stmt ?skip_nested_fdefs f acc top) init prog

let fold_exprs_in_program ?skip_nested_fdefs
    (f : 'acc -> G.expr -> 'acc) (init : 'acc) (prog : G.program) : 'acc =
  List.fold_left (fun acc top ->
    fold_exprs_in_stmt ?skip_nested_fdefs f acc top) init prog

(* Covers parameter defaults, the return-type annotation, and the body;
   walks them directly so the outer fdef itself doesn't trip the
   nested-fdef skip. *)
let fold_exprs_in_fdef ?(skip_nested_fdefs = false)
    (f : 'acc -> G.expr -> 'acc) (init : 'acc)
    (fdef : G.function_definition) : 'acc =
  let acc = ref init in
  let v =
    make_collecting_visitor ~skip_nested_fdefs
      ~on_expr:(fun e -> acc := f !acc e) ()
  in
  v#visit_parameters () fdef.G.fparams;
  Option.iter (v#visit_type_ ()) fdef.G.frettype;
  let body_stmt =
    AST_generic_helpers.funcbody_to_stmt fdef.G.fbody
  in
  v#visit_stmt () body_stmt;
  !acc

(* Full observation list in forward (visitor-traversal) order:
   [Func_def]s first, then the class-like defs. *)
let walk_file ~(lang : Lang.t) (ast : G.program) : Observation.t list =
  let func_defs =
    Visit_function_defs.fold_with_parent_path ~lang
      (fun acc opt_ent parent_path fdef ->
        Observation.Func_def { opt_ent; parent_path; fdef } :: acc)
      [] ast
    |> List.rev
  in
  let class_defs = walk_class_like_defs ast in
  func_defs @ class_defs
