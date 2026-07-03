module G = AST_generic
module Log = Log_projidx.Log

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
  discover_excludes : project_root:Fpath.t -> string list;
  class_def_reshape :
    G.entity -> G.definition_kind -> (G.entity * G.definition_kind) option;
  narrow_methods_by_imports :
    fi_imports:(string * Names.Module_qn.t) list ->
    file_of_func:(Func_info.t -> string option) ->
    Type_state.t ->
    Type_state.t;
  mro_uses_scope_resolution : bool;
  strip_field_sigil : string -> string;
  class_constructor_synth_fields :
    G.function_definition -> (string * G.type_) list;
  (* PHP 8 ctor property promotion: typed ctor params are candidate fields. *)
  ctor_param_promotion : bool;
  interface_dispatch_uses_export_visibility : bool;
}

let decorator_simple_name (a : G.attribute) : string option =
  match a with
  | G.NamedAttr (_, name, _) -> Ty_leaf.leaf_of_name name
  | _ -> None

let entity_simple_name (ent : G.entity) : string option =
  match ent.G.name with
  | G.EN n -> Ty_leaf.leaf_of_name n
  | _ -> None

let name_to_path (n : G.name) : string list =
  match n with
  | G.Id ((s, _), _) -> [s]
  | G.IdQualified { G.name_last = ((s, _), _); name_middle; _ } ->
    let mids =
      match name_middle with
      | Some (G.QDots dots) -> List.map (fun ((s, _), _) -> s) dots
      | _ -> []
    in
    mids @ [s]

let callee_simple_name (callee : G.expr) : string option =
  match callee.G.e with
  | G.N name -> Ty_leaf.leaf_of_name name
  | G.DotAccess (_, _, G.FN name) -> Ty_leaf.leaf_of_name name
  | _ -> None

let first_arg_string (args : G.argument list) : string option =
  match args with
  | G.Arg { G.e = G.L (G.String (_, (s, _), _)); _ } :: _ -> Some s
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
      let s = List.map fst parts |> String.concat "." in
      if String.length s > 0 then Some s else None
    | _ -> None
  ) ast

let extract_clojure_ns_decl (ast : G.program) : string option =
  let name_of_expr (e : G.expr) : string option =
    match e.G.e with
    | G.N n -> Option.map (String.concat ".") (Some (name_to_path n))
    | _ -> None
  in
  let path_of_expr e =
    Option.bind (name_of_expr e) (fun s ->
      if String.length s > 0 then Some s else None)
  in
  List.find_map (fun stmt ->
    match stmt.G.s with
    | G.DirectiveStmt
        { G.d = G.OtherDirective (("NsDirective", _), G.E ns_expr :: _); _ } ->
      path_of_expr ns_expr
    | _ -> None
  ) ast

let strip_c_header_ext (s : string) : string =
  let exts = [".hpp"; ".hxx"; ".hh"; ".h"] in
  match List.find_opt (fun e -> Filename.check_suffix s e) exts with
  | Some e -> Filename.chop_suffix s e
  | None -> s

let python_is_init_file (file : Fpath.t) : bool =
  Filename.basename (Fpath.to_string file) = "__init__.py"

let python_rewrite_module_path (s : string) : string =
  if Filename.basename s = "__init__" then Filename.dirname s else s

let is_dataclass_decorator (a : G.attribute) : bool =
  match decorator_simple_name a with Some "dataclass" -> true | _ -> false

let dataclass_kwarg_is (b : bool) (a : G.attribute) (kw : string) : bool =
  match a with
  | G.NamedAttr (_, _, (_, args, _)) ->
    List.exists (function
      | G.ArgKwd ((s, _), { G.e = G.L (G.Bool (v, _)); _ }) ->
        s = kw && Bool.equal v b
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
  | Some a ->
    let init = not (dataclass_kwarg_is false a "init") in
    let frozen = dataclass_kwarg_is true a "frozen" in
    let acc = if init then ["__init__"] else [] in
    let acc = "__replace__" :: acc in
    if frozen then "__hash__" :: acc else acc

let python_namedtuple_dunders =
  ["__init__"; "__new__"; "__iter__"; "__hash__"; "__replace__"]

let is_namedtuple_subclass (cdef : G.class_definition) : bool =
  List.exists (fun (ty, _) ->
    match ty.G.t with
    | G.TyN n -> Ty_leaf.leaf_of_name n = Some "NamedTuple"
    | _ -> false
  ) cdef.G.cextends

let python_class_dunders_from_extends cdef =
  if is_namedtuple_subclass cdef then python_namedtuple_dunders else []

let newtype_call e = is_call_to "NewType" e
let namedtuple_call e = is_call_to "namedtuple" e
let enum_call e = is_call_to "Enum" e

let python_synth_call_dunders (e : G.expr) : string list option =
  if namedtuple_call e then Some python_namedtuple_dunders
  else if newtype_call e || enum_call e then Some []
  else None

let python_inner_class_from_call (e : G.expr)
  : (string * string list) option =
  match call_first_string_arg e with
  | None -> None
  | Some n ->
    if namedtuple_call e then Some (n, python_namedtuple_dunders)
    else if newtype_call e || enum_call e then Some (n, [])
    else None

let python_extract_wrapper (ent : G.entity) : wrapper option =
  let frozens =
    List.filter_map dataclass_transform_frozen_default ent.G.attrs
  in
  match frozens, entity_simple_name ent with
  | [], _ | _, None -> None
  | _, Some name ->
    let frozen_default = List.exists (fun f -> f) frozens in
    Some { w_simple_name = name; w_frozen_default = frozen_default }

let python_wrapper_dunders (w : wrapper) : string list =
  let dunders = ["__init__"; "__replace__"] in
  if w.w_frozen_default then dunders @ ["__hash__"] else dunders

let strip_jsonc (s : string) : string =
  let n = String.length s in
  let buf = Buffer.create n in
  let rec loop i state =
    if i >= n then ()
    else
      let c = s.[i] in
      match state with
      | `Line_cmt ->
        if c = '\n' then Buffer.add_char buf c;
        loop (i + 1) (if c = '\n' then `Normal else `Line_cmt)
      | `Block_cmt ->
        if c = '*' && i + 1 < n && s.[i + 1] = '/' then loop (i + 2) `Normal
        else begin
          if c = '\n' then Buffer.add_char buf c;
          loop (i + 1) `Block_cmt
        end
      | `In_string escaping ->
        Buffer.add_char buf c;
        let next_state =
          if escaping then `In_string false
          else if c = '\\' then `In_string true
          else if c = '"' then `Normal
          else `In_string false
        in
        loop (i + 1) next_state
      | `Normal ->
        if c = '"' then begin
          Buffer.add_char buf c; loop (i + 1) (`In_string false)
        end else if c = '/' && i + 1 < n then begin
          let next = s.[i + 1] in
          if next = '/' then loop (i + 2) `Line_cmt
          else if next = '*' then loop (i + 2) `Block_cmt
          else begin Buffer.add_char buf c; loop (i + 1) `Normal end
        end else begin
          Buffer.add_char buf c; loop (i + 1) `Normal
        end
  in
  loop 0 `Normal;
  Buffer.contents buf

let strip_trailing_commas (s : string) : string =
  let re = Re.compile
    (Re.seq [Re.char ','; Re.rep (Re.set " \t\n\r"); Re.set "]}"]) in
  Re.replace re ~f:(fun g ->
    let m = Re.Group.get g 0 in
    String.sub m 1 (String.length m - 1)) s

let read_tsconfig_excludes (path : Fpath.t) : string list =
  match
    Nonfatal.catch ~default:None (fun () ->
      Some (UFile.read_file path))
  with
  | None ->
    Log.debug (fun m ->
      m "tsconfig: failed to read %s; no excludes applied"
        (Fpath.to_string path));
    []
  | Some raw ->
  Nonfatal.catch ~default:[] (fun () ->
    let cleaned = raw |> strip_jsonc |> strip_trailing_commas in
    let json = Yojson.Basic.from_string cleaned in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "exclude" fields with
       | Some (`List items) ->
         List.filter_map (function `String s -> Some s | _ -> None) items
       | _ -> [])
    | _ -> [])

(* Prefer tsconfig.build.json over tsconfig.json to match scip-typescript. *)
let find_tsconfigs (project_root : Fpath.t) : Fpath.t list =
  let root_str = Fpath.to_string project_root in
  let skip_dir name =
    name = "node_modules" || name = ".git" || name = ".yarn"
    || name = "dist" || name = "build" || name = ".cache"
  in
  (* Depth cap guards against cyclic directory symlinks. *)
  let max_depth = 64 in
  let rec walk (depth : int) (dir : string) (acc : Fpath.t list) : Fpath.t list =
    if depth > max_depth then acc
    else
    let entries =
      Nonfatal.catch ~default:[] (fun () ->
        Sys.readdir dir |> Array.to_list)
    in
    let build_path = Filename.concat dir "tsconfig.build.json" in
    let plain_path = Filename.concat dir "tsconfig.json" in
    let acc =
      if Sys.file_exists build_path then Fpath.v build_path :: acc
      else if Sys.file_exists plain_path then Fpath.v plain_path :: acc
      else acc
    in
    List.fold_left (fun acc e ->
      let full = Filename.concat dir e in
      let is_dir = Nonfatal.catch ~default:false (fun () -> Sys.is_directory full) in
      if is_dir && not (skip_dir e) then walk (depth + 1) full acc
      else acc
    ) acc entries
  in
  walk 0 root_str []

let normalize_pattern ~(project_root : Fpath.t) ~(config_dir : Fpath.t)
    (pat : string) : string =
  if String.length pat > 0 && pat.[0] = '/' then pat
  else
    let cfg_rel =
      Nonfatal.catch ~default:"" (fun () ->
        let pr = Fpath.to_string project_root in
        let cd = Fpath.to_string config_dir in
        if String.length cd >= String.length pr
           && String.sub cd 0 (String.length pr) = pr
        then
          let rel = String.sub cd (String.length pr) (String.length cd - String.length pr) in
          if String.length rel > 0 && rel.[0] = '/' then String.sub rel 1 (String.length rel - 1)
          else rel
        else "")
    in
    if cfg_rel = "" then pat
    else cfg_rel ^ "/" ^ pat

let typescript_discover_excludes ~(project_root : Fpath.t) : string list =
  let configs = find_tsconfigs project_root in
  List.concat_map (fun cfg ->
    let dir = Fpath.parent cfg in
    let raw = read_tsconfig_excludes cfg in
    List.map (normalize_pattern ~project_root ~config_dir:dir) raw)
    configs

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
  discover_excludes = (fun ~project_root:_ -> []);
  class_def_reshape = (fun _ _ -> None);
  narrow_methods_by_imports =
    (fun ~fi_imports:_ ~file_of_func:_ ts -> ts);
  strip_field_sigil = (fun s -> s);
  class_constructor_synth_fields = (fun _ -> []);
  ctor_param_promotion = false;
  mro_uses_scope_resolution = false;
  interface_dispatch_uses_export_visibility = false;
}

let string_contains (s : string) (sub : string) : bool =
  let n = String.length s and m = String.length sub in
  let rec loop i =
    if i + m > n then false
    else if String.equal (String.sub s i m) sub then true
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
    Hashtbl.fold (fun cls hint s ->
      let cls_name = Names.Class_name.of_string cls in
      match Type_state.get_methods s cls_name with
      | None -> s
      | Some methods ->
        let hint_dash =
          String.map (fun c -> if c = '_' then '-' else c) hint
        in
        let filtered = List.filter (fun (f : Func_info.t) ->
          match file_of_func f with
          | None -> false
          | Some file ->
            string_contains file hint || string_contains file hint_dash
        ) methods in
        if filtered <> [] && List.length filtered <> List.length methods
        then Type_state.set_methods s cls_name filtered
        else s
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

(* Token points at the symbol literal so def-site location matches scip-ruby. *)
let ruby_class_body_synth_methods (cdef : G.class_definition)
  : (string * Tok.t) list =
  let names_from_call (e : G.expr) : (string * Tok.t) list =
    match e.G.e with
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
    | G.Arg { e = G.N n; _ } -> Some (name_to_path n)
    | _ -> None
  in
  let paths_from_call (e : G.expr) : string list list =
    match e.G.e with
    | G.Call ({ e = G.N (G.Id ((macro, _), _)); _ }, (_, args, _))
      when macro = "include" || macro = "extend" || macro = "prepend" ->
      List.filter_map (fun arg ->
        match arg_to_path arg with
        | Some p when p <> [] -> Some p
        | _ -> None
      ) args
    | _ -> []
  in
  scan_class_body paths_from_call cdef

let ruby_class_def_reshape (ent : G.entity) (def_kind : G.definition_kind)
  : (G.entity * G.definition_kind) option =
  match def_kind with
  | G.ModuleDef { G.mbody = G.ModuleStruct (_, items); _ } ->
    let fk = Tok.unsafe_fake_tok "module" in
    let cdef = G.ClassDef {
      G.ckind = (G.Class, fk);
      cextends = []; cimplements = []; cmixins = [];
      cparams = (fk, [], fk);
      cbody = (fk, List.map (fun s -> G.F s) items, fk);
    } in
    Some (ent, cdef)
  | _ -> None

let ruby : t = { default with
  walks_inheritance = true;
  include_anonymous_funcs = false;
  class_body_synth_methods = ruby_class_body_synth_methods;
  class_body_extra_parents = ruby_class_body_extra_parents;
  class_def_reshape = ruby_class_def_reshape;
  mro_uses_scope_resolution = true;
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
  discover_excludes = typescript_discover_excludes;
  class_constructor_synth_fields = typescript_class_constructor_synth_fields;
}

let php_strip_field_sigil (s : string) : string =
  if String.length s > 0 && s.[0] = '$'
  then String.sub s 1 (String.length s - 1)
  else s

let php : t = { default with
  walks_inheritance = true;
  include_anonymous_funcs = false;
  strip_field_sigil = php_strip_field_sigil;
  ctor_param_promotion = true;
}

let rust_class_def_reshape (ent : G.entity) (def_kind : G.definition_kind)
  : (G.entity * G.definition_kind) option =
  match def_kind with
  | G.OtherDef ((kind, _), anys) when String.equal kind "Impl" ->
    let ty_opt =
      List.find_map (function G.T t -> Some t | _ -> None) anys
    in
    let stmts =
      List.concat_map (function G.Ss s -> s | _ -> []) anys
    in
    (match ty_opt with
     | Some { G.t = G.TyN (G.Id _ as n); _ }
     | Some { G.t = G.TyExpr { G.e = G.N (G.Id _ as n); _ }; _ } ->
       let new_ent = { ent with G.name = G.EN n } in
       let fk = Tok.unsafe_fake_tok "impl" in
       let cdef = G.ClassDef {
         G.ckind = (G.Class, fk);
         cextends = []; cimplements = []; cmixins = [];
         cparams = (fk, [], fk);
         cbody = (fk, List.map (fun s -> G.F s) stmts, fk);
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
  | _ -> default
