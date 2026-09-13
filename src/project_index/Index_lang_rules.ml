module G = AST_generic

type wrapper = {
  w_simple_name : string;
  w_frozen_default : bool;
}

type project_discovery = {
  excludes : string list;
  module_paths : (string * string list) list;
}

type parent_position = Prepended | Appended

type class_parent = {
  cp_path : string list;
  cp_position : parent_position;
}

type singleton_exposure =
  | No_singleton_exposure
  | Every_method_is_a_singleton
  | Named_singleton_methods of string list

type parent_resolution =
  | Parent_in_own_scope
  | Parent_by_lexical_scope
  | Parent_by_lexical_scope_then_homonym

type t = {
  is_init_file : Fpath.t -> bool;
  is_stub_file : Fpath.t -> bool;
  rewrite_module_path : string -> string;
  module_path_from_ast : G.program -> string option;
  normalize_import_specifier : string -> string;
  class_dunders_from_decorators : G.attribute list -> string list;
  class_dunders_from_extends : G.class_definition -> string list;
  synth_call_dunders : G.expr -> string list option;
  inner_class_from_call : G.expr -> (string * string list) option;
  class_body_synth_methods : G.class_definition -> (string * Tok.t) list;
  class_body_extra_parents : G.class_definition -> class_parent list;
  class_body_singleton_methods : G.class_definition -> singleton_exposure;
  extract_wrapper : G.entity -> wrapper option;
  wrapper_dunders : wrapper -> string list;
  walks_inheritance : bool;
  has_reexports : bool;
  include_anonymous_funcs : bool;
  unqualified_scope :
    [ `Per_file | `Per_directory | `Per_package | `Per_namespace
    | `Per_module | `Per_go_package | `Per_constant_path ];
  (* This language's [Package]/[PackageEnd] directives ([namespace] blocks in
     C++/PHP, [package] clauses in Java/Kotlin/Scala) are qn scopes: a class is
     qualified by the region open at its definition, so several or nested
     namespaces per file are attributed correctly.  False where a [package]
     directive names the file's module identity instead (Go, via go.mod). *)
  package_directive_is_namespace : bool;
  (* A class's identity is its constant path, independent of the file it is
     (re)opened in (Ruby: [::Base], [Svc::Base]).  Drops the file-path prefix
     from class qns so a class reopened across files shares one qn and parent
     resolution scores on the lexical constant path, not the filename.  The
     file's [module_path] is still tracked (for require-relative / indexing) —
     only the class qn drops it. *)
  class_identity_is_constant_path : bool;
  discover_project : project_root:Fpath.t -> project_discovery;
  class_def_reshape :
    G.entity -> G.definition_kind -> (G.entity * G.definition_kind) option;
  narrow_methods_by_imports :
    fi_imports:(string * Names.Module_qn.t) list ->
    file_of_func:(Func_info.t -> string option) ->
    Type_state.t ->
    Type_state.t;
  strip_field_sigil : string -> string;
  class_constructor_synth_fields :
    G.function_definition -> (string * G.type_) list;
  (* PHP 8 ctor property promotion: typed ctor params are candidate fields. *)
  ctor_param_promotion : bool;
  interface_dispatch_uses_export_visibility : bool;
  parent_resolution : parent_resolution;
  package_clause_of_ast : G.program -> string option;
  method_owner_of_funcdef : G.function_definition -> string option;
  name_is_exported : string -> bool;
}

let equal_parent_position (left : parent_position) (right : parent_position)
    : bool =
  match (left, right) with
  | Prepended, Prepended
  | Appended, Appended -> true
  | (Prepended | Appended), _ -> false

let decorator_simple_name (attr : G.attribute) : string option =
  match attr with
  | G.NamedAttr (_, name, _) -> Ty_bare_name.bare_name_of_name name
  | _ -> None

let entity_simple_name (ent : G.entity) : string option =
  match ent.G.name with
  | G.EN name -> Ty_bare_name.bare_name_of_name name
  | _ -> None

let name_to_path (name : G.name) : string list =
  match name with
  | G.Id ((str, _), _) -> [str]
  | G.IdQualified { G.name_last = ((last_str, _), _); name_middle; _ } ->
    let mids =
      match name_middle with
      | Some (G.QDots dots) -> List.map (fun ((mid_str, _), _) -> mid_str) dots
      | _ -> []
    in
    mids @ [last_str]

let callee_simple_name (callee : G.expr) : string option =
  match callee.G.e with
  | G.N name -> Ty_bare_name.bare_name_of_name name
  | G.DotAccess (_, _, G.FN name) -> Ty_bare_name.bare_name_of_name name
  | _ -> None

let first_arg_string (args : G.argument list) : string option =
  match args with
  | G.Arg { G.e = G.L (G.String (_, (str, _), _)); _ } :: _ -> Some str
  | _ -> None

let is_call_to (target : string) (e : G.expr) : bool =
  match e.G.e with
  | G.Call (callee, _) -> callee_simple_name callee = Some target
  | _ -> false

let call_first_string_arg (e : G.expr) : string option =
  match e.G.e with
  | G.Call (_, (_, args, _)) -> first_arg_string args
  | _ -> None

let extract_package_decl (ast : G.program) : string option =
  List.find_map (fun stmt ->
    match stmt.G.s with
    | G.DirectiveStmt { G.d = G.Package (_, parts); _ } ->
      let pkg = List.map fst parts |> String.concat "." in
      if String.length pkg > 0 then Some pkg else None
    | _ -> None
  ) ast

let extract_clojure_ns_decl (ast : G.program) : string option =
  let name_of_expr (expr : G.expr) : string option =
    match expr.G.e with
    | G.N name -> Option.map (String.concat ".") (Some (name_to_path name))
    | _ -> None
  in
  let path_of_expr expr =
    Option.bind (name_of_expr expr) (fun str ->
      if String.length str > 0 then Some str else None)
  in
  List.find_map (fun stmt ->
    match stmt.G.s with
    | G.DirectiveStmt
        { G.d = G.OtherDirective (("NsDirective", _), G.E ns_expr :: _); _ } ->
      path_of_expr ns_expr
    | _ -> None
  ) ast

let strip_c_header_ext (fname : string) : string =
  let exts = [".hpp"; ".hxx"; ".hh"; ".h"] in
  match List.find_opt (fun ext -> Filename.check_suffix fname ext) exts with
  | Some ext -> Filename.chop_suffix fname ext
  | None -> fname

let python_is_init_file (file : Fpath.t) : bool =
  Filename.basename (Fpath.to_string file) = "__init__.py"

let python_is_stub_file (file : Fpath.t) : bool =
  match Fpath.get_ext file with
  | ".pyi" -> true
  | _ -> false

let python_rewrite_module_path (path : string) : string =
  if Filename.basename path = "__init__" then Filename.dirname path else path

let is_dataclass_decorator (attr : G.attribute) : bool =
  match decorator_simple_name attr with Some "dataclass" -> true | _ -> false

let dataclass_kwarg_is (expected : bool) (attr : G.attribute) (kw : string) : bool =
  match attr with
  | G.NamedAttr (_, _, (_, args, _)) ->
    List.exists (function
      | G.ArgKwd ((key, _), { G.e = G.L (G.Bool (value, _)); _ }) ->
        let open Common in key = kw && Bool.equal value expected
      | _ -> false) args
  | _ -> false

let dataclass_transform_frozen_default (a : G.attribute) : bool option =
  match decorator_simple_name a, a with
  | Some "dataclass_transform", G.NamedAttr (_, _, (_, args, _)) ->
    let frozen = List.exists (function
      | G.ArgKwd (("frozen_default", _),
                  { G.e = G.L (G.Bool (true, _)); _ }) -> true
      | _ -> false) args in
    Some frozen
  | _ -> None

(* Dunders pyrefly synthesises onto a [@dataclass] class. *)
let python_dataclass_dunders (attrs : G.attribute list) : string list =
  match List.find_opt is_dataclass_decorator attrs with
  | None -> []
  | Some attr ->
    let init = not (dataclass_kwarg_is false attr "init") in
    let frozen = dataclass_kwarg_is true attr "frozen" in
    let acc = if init then ["__init__"] else [] in
    let acc = "__replace__" :: acc in
    if frozen then "__hash__" :: acc else acc

let python_namedtuple_dunders =
  ["__init__"; "__new__"; "__iter__"; "__hash__"; "__replace__"]

let is_namedtuple_subclass (cdef : G.class_definition) : bool =
  List.exists (fun (ty, _) ->
    match ty.G.t with
    | G.TyN name -> Ty_bare_name.bare_name_of_name name = Some "NamedTuple"
    | _ -> false
  ) cdef.G.cextends

let python_class_dunders_from_extends cdef =
  if is_namedtuple_subclass cdef then python_namedtuple_dunders else []

let newtype_call expr = is_call_to "NewType" expr
let namedtuple_call expr = is_call_to "namedtuple" expr
let enum_call expr = is_call_to "Enum" expr

let python_synth_call_dunders (expr : G.expr) : string list option =
  if namedtuple_call expr then Some python_namedtuple_dunders
  else if newtype_call expr || enum_call expr then Some []
  else None

let python_inner_class_from_call (expr : G.expr)
  : (string * string list) option =
  match call_first_string_arg expr with
  | None -> None
  | Some name ->
    if namedtuple_call expr then Some (name, python_namedtuple_dunders)
    else if newtype_call expr || enum_call expr then Some (name, [])
    else None

let python_extract_wrapper (ent : G.entity) : wrapper option =
  let frozens =
    List.filter_map dataclass_transform_frozen_default ent.G.attrs
  in
  match frozens, entity_simple_name ent with
  | [], _ | _, None -> None
  | _, Some name ->
    let frozen_default = List.exists (fun frozen -> frozen) frozens in
    Some { w_simple_name = name; w_frozen_default = frozen_default }

let python_wrapper_dunders (wrapper : wrapper) : string list =
  let dunders = ["__init__"; "__replace__"] in
  if wrapper.w_frozen_default then dunders @ ["__hash__"] else dunders

let default : t = {
  is_init_file = (fun _ -> false);
  is_stub_file = (fun _ -> false);
  rewrite_module_path = (fun s -> s);
  module_path_from_ast = (fun _ -> None);
  normalize_import_specifier = (fun s -> s);
  class_dunders_from_decorators = (fun _ -> []);
  class_dunders_from_extends = (fun _ -> []);
  synth_call_dunders = (fun _ -> None);
  inner_class_from_call = (fun _ -> None);
  extract_wrapper = (fun _ -> None);
  wrapper_dunders = (fun _ -> []);
  walks_inheritance = false;
  has_reexports = false;
  include_anonymous_funcs = true;
  class_body_synth_methods = (fun _ -> []);
  class_body_extra_parents = (fun _ -> []);
  class_body_singleton_methods = (fun _ -> No_singleton_exposure);
  unqualified_scope = `Per_file;
  package_directive_is_namespace = false;
  class_identity_is_constant_path = false;
  discover_project =
    (fun ~project_root:_ -> { excludes = []; module_paths = [] });
  class_def_reshape = (fun _ _ -> None);
  narrow_methods_by_imports =
    (fun ~fi_imports:_ ~file_of_func:_ ts -> ts);
  strip_field_sigil = (fun s -> s);
  class_constructor_synth_fields = (fun _ -> []);
  ctor_param_promotion = false;
  interface_dispatch_uses_export_visibility = false;
  parent_resolution = Parent_by_lexical_scope_then_homonym;
  package_clause_of_ast = (fun _ -> None);
  method_owner_of_funcdef = (fun _ -> None);
  name_is_exported = (fun _ -> true);
}

let string_contains (str : string) (sub : string) : bool =
  let n = String.length str and m = String.length sub in
  let rec loop i =
    if i + m > n then false
    else if String.equal (String.sub str i m) sub then true
    else loop (i + 1)
  in
  m > 0 && loop 0

(* Skips when nothing survives so a wrong crate hint never erases a class. *)
let rust_narrow_methods_by_imports
    ~(fi_imports : (string * Names.Module_qn.t) list)
    ~(file_of_func : Func_info.t -> string option)
    (ts : Type_state.t) : Type_state.t =
  let import_hint : (string, string) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (local, target) ->
    match Names.Module_qn.parts target with
    | hint :: _ :: _ when String.length hint > 0 ->
      Hashtbl.replace import_hint local hint
    | _ -> ()
  ) fi_imports;
  if Hashtbl.length import_hint = 0 then ts
  else
    Hashtbl.fold (fun cls hint state ->
      let cls_name = Names.Class_name.of_string cls in
      match Type_state.get_methods state cls_name with
      | None -> state
      | Some methods ->
        let hint_dash =
          String.map (fun ch -> if ch = '_' then '-' else ch) hint
        in
        let keep (func : Func_info.t) : bool =
          match file_of_func func with
          | None -> false
          | Some file ->
            string_contains file hint || string_contains file hint_dash
        in
        Type_state.set_methods state cls_name (Func_info.prefer ~keep methods)
    ) import_hint ts

let python : t = { default with
  is_init_file = python_is_init_file;
  is_stub_file = python_is_stub_file;
  rewrite_module_path = python_rewrite_module_path;
  class_dunders_from_decorators = python_dataclass_dunders;
  class_dunders_from_extends = python_class_dunders_from_extends;
  synth_call_dunders = python_synth_call_dunders;
  inner_class_from_call = python_inner_class_from_call;
  extract_wrapper = python_extract_wrapper;
  wrapper_dunders = python_wrapper_dunders;
  walks_inheritance = true;
  has_reexports = true;
  include_anonymous_funcs = false;
}

(* Ruby class bodies wrap stmts in a single [Block]; walk one level to reach
   the macro calls. *)
let scan_class_body (of_call : G.expr -> 'a list)
    (cdef : G.class_definition) : 'a list =
  let rec scan_stmt (stmt : G.stmt) : 'a list =
    match stmt.G.s with
    | G.ExprStmt (e, _) -> of_call e
    | G.Block (_, stmts, _) -> List.concat_map scan_stmt stmts
    | _ -> []
  in
  let _, fields, _ = cdef.G.cbody in
  List.concat_map (fun (G.F stmt) -> scan_stmt stmt) fields

(* Token points at the symbol literal so each synthesised accessor gets its
   own def-site location. *)
let ruby_class_body_synth_methods (cdef : G.class_definition)
  : (string * Tok.t) list =
  let names_from_call (expr : G.expr) : (string * Tok.t) list =
    match expr.G.e with
    | G.Call ({ e = G.N (G.Id ((macro, _), _)); _ }, (_, args, _))
      when macro = "attr_reader"
           || macro = "attr_writer"
           || macro = "attr_accessor" ->
      List.concat_map (fun arg ->
        match arg with
        | G.Arg { e = G.L (G.Atom (_, (sym, sym_tok))); _ } ->
          let reader = if macro <> "attr_writer" then [(sym, sym_tok)] else [] in
          let writer = if macro <> "attr_reader" then [(sym ^ "=", sym_tok)] else [] in
          reader @ writer
        | _ -> []
      ) args
    | G.Call ({ e = G.N (G.Id (("delegate", _), _)); _ }, (_, args, _)) ->
      List.filter_map (fun arg ->
        match arg with
        | G.Arg { e = G.L (G.Atom (_, (sym, sym_tok))); _ } ->
          Some (sym, sym_tok)
        | _ -> None
      ) args
    | _ -> []
  in
  scan_class_body names_from_call cdef

let ruby_class_body_singleton_methods (cdef : G.class_definition)
    : singleton_exposure =
  let exposure_from_call (expr : G.expr) : singleton_exposure list =
    match expr.G.e with
    | G.Call ({ e = G.N (G.Id (("extend", _), _)); _ },
              (_, [ G.Arg { e = G.IdSpecial (G.Self, _); _ } ], _)) ->
      [ Every_method_is_a_singleton ]
    | G.Call ({ e = G.N (G.Id (("module_function", _), _)); _ }, (_, [], _)) ->
      [ Every_method_is_a_singleton ]
    | G.Call ({ e = G.N (G.Id (("module_function", _), _)); _ },
              (_, (_ :: _ as args), _)) ->
      [ Named_singleton_methods
          (List.filter_map (fun (arg : G.argument) ->
             match arg with
             | G.Arg { e = G.L (G.Atom (_, (name, _))); _ } -> Some name
             | _ -> None)
             args) ]
    | _ -> []
  in
  List.fold_left
    (fun (exposure : singleton_exposure) (found : singleton_exposure) ->
      match (exposure, found) with
      | Every_method_is_a_singleton, _
      | _, Every_method_is_a_singleton -> Every_method_is_a_singleton
      | No_singleton_exposure, _ -> found
      | _, No_singleton_exposure -> exposure
      | Named_singleton_methods (earlier : string list),
        Named_singleton_methods (later : string list) ->
        Named_singleton_methods (earlier @ later))
    No_singleton_exposure
    (scan_class_body exposure_from_call cdef)

let ruby_mixin_position (macro : string) : parent_position option =
  match macro with
  | "prepend" -> Some Prepended
  | "include"
  | "extend" -> Some Appended
  | _ -> None

let ruby_class_body_extra_parents (cdef : G.class_definition)
  : class_parent list =
  let arg_to_path (arg : G.argument) : string list option =
    match arg with
    | G.Arg { e = G.N name; _ } -> Some (name_to_path name)
    | _ -> None
  in
  let paths_from_call (expr : G.expr) : class_parent list =
    match expr.G.e with
    | G.Call ({ e = G.N (G.Id ((macro, _), _)); _ }, (_, args, _)) -> (
      match ruby_mixin_position macro with
      | None -> []
      | Some (position : parent_position) ->
        List.filter_map (fun arg ->
          match arg_to_path arg with
          | Some ((_ :: _) as path) ->
            Some { cp_path = path; cp_position = position }
          | Some []
          | None -> None
        ) args)
    | _ -> []
  in
  scan_class_body paths_from_call cdef

let ruby : t = { default with
  walks_inheritance = true;
  include_anonymous_funcs = false;
  class_body_synth_methods = ruby_class_body_synth_methods;
  class_body_extra_parents = ruby_class_body_extra_parents;
  class_body_singleton_methods = ruby_class_body_singleton_methods;
  (* A Ruby class IS its constant path; files are irrelevant (reopening). *)
  class_identity_is_constant_path = true;
  unqualified_scope = `Per_constant_path;
  parent_resolution = Parent_by_lexical_scope;
}

let go_class_of_fields (kind : G.class_kind) (fk : Tok.t)
    (fields : G.field list) : G.definition_kind =
  G.ClassDef {
    G.ckind = (kind, fk);
    cextends = []; cimplements = []; cmixins = [];
    cparams = (fk, [], fk);
    cbody = (fk, fields, fk);
  }

let go_class_def_reshape (ent : G.entity) (def_kind : G.definition_kind)
  : (G.entity * G.definition_kind) option =
  match def_kind with
  | G.TypeDef
      { G.tbody = G.NewType
          { G.t = G.TyRecordAnon ((kind, fk), (_, fields, _)); _ } }
    when (match kind with G.Class | G.Interface -> true | _ -> false) ->
    Some (ent, go_class_of_fields kind fk fields)
  | G.TypeDef { G.tbody = G.NewType (ty : G.type_) } ->
    let fk =
      match AST_generic_helpers.range_of_any_opt (G.T ty) with
      | Some ((start_tok : Tok.location), _) -> Tok.tok_of_loc start_tok
      | None -> Tok.unsafe_fake_tok "type"
    in
    Some (ent, go_class_of_fields G.Class fk [])
  | _ -> None

let go_class_body_extra_parents (cdef : G.class_definition) : class_parent list =
  Tok.unbracket cdef.G.cbody
  |> List.filter_map (fun (field : G.field) ->
    match field with
    | G.F { G.s = G.ExprStmt (
        { G.e = G.Call ({ G.e = G.IdSpecial (G.Spread, _); _ },
                        (_, [ G.Arg { G.e = G.N (name : G.name); _ } ], _)); _ },
        _); _ } -> (
      match name_to_path name with
      | [] -> None
      | (path : string list) ->
        Some { cp_path = path; cp_position = Appended })
    | _ -> None)

let go_method_owner_of_funcdef (fdef : G.function_definition) : string option =
  match Tok.unbracket fdef.G.fparams with
  | G.ParamReceiver { G.ptype = Some (ty : G.type_); _ } :: _ ->
    Option.bind (Ty_bare_name.inner_class_name_of_ty ty)
      Ty_bare_name.bare_name_of_name
  | _ -> None

let is_ascii_lower (char : char) : bool =
  Char.equal char (Char.lowercase_ascii char)
  && not (Char.equal char (Char.uppercase_ascii char))

let go_name_is_exported (name : string) : bool =
  String.length name > 0
  &&
  let first = Uchar.utf_decode_uchar (String.get_utf_8_uchar name 0) in
  if Uchar.is_char first then
    let char = Uchar.to_char first in
    (not (is_ascii_lower char)) && not (Char.equal char '_')
  else true

let go : t = { default with
  include_anonymous_funcs = false;
  name_is_exported = go_name_is_exported;
  unqualified_scope = `Per_go_package;
  method_owner_of_funcdef = go_method_owner_of_funcdef;
  class_def_reshape = go_class_def_reshape;
  class_body_extra_parents = go_class_body_extra_parents;
  walks_inheritance = true;
  parent_resolution = Parent_in_own_scope;
  package_clause_of_ast = extract_package_decl;
  interface_dispatch_uses_export_visibility = true;
}

let typescript_class_constructor_synth_fields
    (fdef : G.function_definition) : (string * G.type_) list =
  let is_param_property_attr (attr : G.attribute) : bool =
    match attr with
    | G.KeywordAttr (kw, _) ->
      (match kw with
       | G.Private | G.Public | G.Protected | G.Mutable -> true
       | _ -> false)
    | _ -> false
  in
  Tok.unbracket fdef.G.fparams
  |> List.filter_map (function
    | G.Param { pname = Some (pn, _); ptype = Some pty; pattrs; _ }
      when List.exists is_param_property_attr pattrs -> Some (pn, pty)
    | _ -> None)

let typescript : t = { default with
  walks_inheritance = true;
  include_anonymous_funcs = false;
  discover_project =
    (fun ~(project_root : Fpath.t) ->
      let excludes, module_paths = Ts_modules.discover ~project_root in
      { excludes; module_paths });
  class_constructor_synth_fields = typescript_class_constructor_synth_fields;
  unqualified_scope = `Per_module;
  package_directive_is_namespace = true;
}

let php_strip_field_sigil (field : string) : string =
  if String.length field > 0 && field.[0] = '$'
  then String.sub field 1 (String.length field - 1)
  else field

let php_namespace_decl (ast : G.program) : string option =
  Some (Option.value (extract_package_decl ast) ~default:"")

let php_class_body_extra_parents (cdef : G.class_definition)
  : class_parent list =
  List.filter_map
    (fun (ty : G.type_) ->
      match ty.G.t with
      | G.TyN (name : G.name) -> (
        match name_to_path name with
        | [] -> None
        | (path : string list) ->
          Some { cp_path = path; cp_position = Appended })
      | _ -> None)
    cdef.G.cmixins

let php : t = { default with
  walks_inheritance = true;
  include_anonymous_funcs = false;
  strip_field_sigil = php_strip_field_sigil;
  ctor_param_promotion = true;
  (* PHP [namespace App\Svc;] parses to [Package]/[PackageEnd]. *)
  package_directive_is_namespace = true;
  unqualified_scope = `Per_namespace;
  module_path_from_ast = php_namespace_decl;
  class_body_extra_parents = php_class_body_extra_parents;
}

(* Scala [package a.b] (and nested [package a { package b {..} }]) parse to
   [Package]/[PackageEnd].  [object] is a [ClassDef] (kind [Object]), not a
   [ModuleDef], so [walks_inheritance] only affects classes with [extends]. *)
let scala : t = { default with
  package_directive_is_namespace = true;
  walks_inheritance = true;
}

let rust_class_def_reshape (ent : G.entity) (def_kind : G.definition_kind)
  : (G.entity * G.definition_kind) option =
  match def_kind with
  | G.OtherDef ((kind, _), anys) when String.equal kind "Impl" ->
    let ty_opt =
      List.find_map (function G.T ty -> Some ty | _ -> None) anys
    in
    let stmts =
      List.concat_map (function G.Ss body -> body | _ -> []) anys
    in
    (match ty_opt with
     | Some { G.t = G.TyN (G.Id _ as name); _ }
     | Some { G.t = G.TyExpr { G.e = G.N (G.Id _ as name); _ }; _ } ->
       let new_ent = { ent with G.name = G.EN name } in
       let fk = Tok.unsafe_fake_tok "impl" in
       let cdef = G.ClassDef {
         G.ckind = (G.Class, fk);
         cextends = []; cimplements = []; cmixins = [];
         cparams = (fk, [], fk);
         cbody = (fk, List.map (fun stmt -> G.F stmt) stmts, fk);
       } in
       Some (new_ent, cdef)
     | _ -> None)
  | _ -> None

let rust : t = { default with
  narrow_methods_by_imports = rust_narrow_methods_by_imports;
  class_def_reshape = rust_class_def_reshape;
}

(* Package-scoped languages: a type/class lives in a package, resolved by
   the package declaration rather than the file path. *)
let package_scoped : t = { default with
  walks_inheritance = true;
  include_anonymous_funcs = false;
  unqualified_scope = `Per_package;
  package_directive_is_namespace = true;
  module_path_from_ast = extract_package_decl;
}

let java : t = package_scoped
let kotlin : t = package_scoped
let csharp : t = package_scoped
let cpp : t =
  { package_scoped with normalize_import_specifier = strip_c_header_ext }

let c : t = { default with
  unqualified_scope = `Per_directory;
  normalize_import_specifier = strip_c_header_ext;
}

let clojure : t = { default with
  module_path_from_ast = extract_clojure_ns_decl;
}

let for_lang (lang : Lang.t) : t =
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3 -> python
  | Lang.Ruby -> ruby
  | Lang.Go -> go
  | Lang.Ts | Lang.Js -> typescript
  | Lang.Php -> php
  | Lang.Rust -> rust
  | Lang.Java -> java
  | Lang.Kotlin -> kotlin
  | Lang.Csharp -> csharp
  | Lang.Cpp -> cpp
  | Lang.C -> c
  | Lang.Clojure -> clojure
  | Lang.Scala -> scala
  | _ -> default
