module G = AST_generic

type wrapper = {
  w_simple_name : string;
  w_frozen_default : bool;
}

type t = {
  is_init_file : Fpath.t -> bool;
  rewrite_module_path : string -> string;
  module_path_from_ast : G.program -> string option;
  normalize_import_specifier : string -> string;
  class_dunders_from_decorators : G.attribute list -> string list;
  class_dunders_from_extends : G.class_definition -> string list;
  synth_call_dunders : G.expr -> string list option;
  inner_class_from_call : G.expr -> (string * string list) option;
  class_body_synth_methods : G.class_definition -> (string * Tok.t) list;
  class_body_extra_parents : G.class_definition -> string list list;
  extract_wrapper : G.entity -> wrapper option;
  wrapper_dunders : wrapper -> string list;
  walks_inheritance : bool;
  has_reexports : bool;
  include_anonymous_funcs : bool;
  unqualified_scope : [ `Per_file | `Per_directory | `Per_package ];
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
  discover_excludes : project_root:Fpath.t -> string list;
  class_def_reshape :
    G.entity -> G.definition_kind -> (G.entity * G.definition_kind) option;
  narrow_methods_by_imports :
    fi_imports:(string * Names.Module_qn.t) list ->
    file_of_func:(Func_info.t -> string option) ->
    Type_state.t ->
    Type_state.t;
  (* When true, restrict each imported class's methods to the file(s) it was
     actually imported from (resolved via the path-suffix index).  Disambiguates
     same-named classes across files at method dispatch — e.g. TS/JS default
     imports where two files each `export default class Handler`. *)
  narrow_methods_by_import_files : bool;
  (* When true, restrict same-named colliding methods to the files the caller
     itself requires (whole-file "*" import specifiers — Ruby
     [require_relative], PHP [require]/[include]).  These languages bind no
     local name per import, so the narrowing keys on the caller's required
     files rather than on an imported class name. *)
  narrow_methods_by_required_files : bool;
  strip_field_sigil : string -> string;
  class_constructor_synth_fields :
    G.function_definition -> (string * G.type_) list;
  (* PHP 8 ctor property promotion: typed ctor params are candidate fields. *)
  ctor_param_promotion : bool;
  interface_dispatch_uses_export_visibility : bool;
}

let decorator_simple_name (attr : G.attribute) : string option =
  match attr with
  | G.NamedAttr (_, name, _) -> Ty_leaf.leaf_of_name name
  | _ -> None

let entity_simple_name (ent : G.entity) : string option =
  match ent.G.name with
  | G.EN name -> Ty_leaf.leaf_of_name name
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
  | G.N name -> Ty_leaf.leaf_of_name name
  | G.DotAccess (_, _, G.FN name) -> Ty_leaf.leaf_of_name name
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
    | G.TyN name -> Ty_leaf.leaf_of_name name = Some "NamedTuple"
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
  unqualified_scope = `Per_file;
  package_directive_is_namespace = false;
  class_identity_is_constant_path = false;
  discover_excludes = (fun ~project_root:_ -> []);
  class_def_reshape = (fun _ _ -> None);
  narrow_methods_by_imports =
    (fun ~fi_imports:_ ~file_of_func:_ ts -> ts);
  narrow_methods_by_import_files = false;
  narrow_methods_by_required_files = false;
  strip_field_sigil = (fun s -> s);
  class_constructor_synth_fields = (fun _ -> []);
  ctor_param_promotion = false;
  interface_dispatch_uses_export_visibility = false;
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
        let filtered = List.filter (fun (func : Func_info.t) ->
          match file_of_func func with
          | None -> false
          | Some file ->
            string_contains file hint || string_contains file hint_dash
        ) methods in
        if filtered <> [] && List.length filtered <> List.length methods
        then Type_state.set_methods state cls_name filtered
        else state
    ) import_hint ts

let python : t = { default with
  is_init_file = python_is_init_file;
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

let ruby_class_body_extra_parents (cdef : G.class_definition)
  : string list list =
  let arg_to_path (arg : G.argument) : string list option =
    match arg with
    | G.Arg { e = G.N name; _ } -> Some (name_to_path name)
    | _ -> None
  in
  let paths_from_call (expr : G.expr) : string list list =
    match expr.G.e with
    | G.Call ({ e = G.N (G.Id ((macro, _), _)); _ }, (_, args, _))
      when macro = "include" || macro = "extend" || macro = "prepend" ->
      List.filter_map (fun arg ->
        match arg_to_path arg with
        | Some path when path <> [] -> Some path
        | _ -> None
      ) args
    | _ -> []
  in
  scan_class_body paths_from_call cdef

let ruby : t = { default with
  walks_inheritance = true;
  include_anonymous_funcs = false;
  class_body_synth_methods = ruby_class_body_synth_methods;
  class_body_extra_parents = ruby_class_body_extra_parents;
  (* A Ruby class IS its constant path; files are irrelevant (reopening). *)
  class_identity_is_constant_path = true;
  narrow_methods_by_required_files = true;
}

let go_class_def_reshape (ent : G.entity) (def_kind : G.definition_kind)
  : (G.entity * G.definition_kind) option =
  match def_kind with
  | G.TypeDef
      { G.tbody = G.NewType
          { G.t = G.TyRecordAnon ((kind, fk), (_, fields, _)); _ } }
    when (match kind with G.Class | G.Interface -> true | _ -> false) ->
    let cdef = G.ClassDef {
      G.ckind = (kind, fk);
      cextends = []; cimplements = []; cmixins = [];
      cparams = (fk, [], fk);
      cbody = (fk, fields, fk);
    } in
    Some (ent, cdef)
  | _ -> None

let go : t = { default with
  include_anonymous_funcs = false;
  unqualified_scope = `Per_directory;
  class_def_reshape = go_class_def_reshape;
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
  include_anonymous_funcs = false;
  discover_excludes = Ts_modules.discover_excludes;
  class_constructor_synth_fields = typescript_class_constructor_synth_fields;
  narrow_methods_by_import_files = true;
}

let php_strip_field_sigil (field : string) : string =
  if String.length field > 0 && field.[0] = '$'
  then String.sub field 1 (String.length field - 1)
  else field

let php : t = { default with
  walks_inheritance = true;
  include_anonymous_funcs = false;
  strip_field_sigil = php_strip_field_sigil;
  ctor_param_promotion = true;
  (* PHP [namespace App\Svc;] parses to [Package]/[PackageEnd]. *)
  package_directive_is_namespace = true;
  narrow_methods_by_required_files = true;
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
