module G = AST_generic

open Types

let named_function (init : G.expr) : string option =
  match init.G.e with
  | G.N (G.Id (((name : string), _), _)) -> Some name
  | G.Ref (_, { G.e = G.N (G.Id (((name : string), _), _)); _ }) -> Some name
  | _ -> None

let file_scope_values (ast : G.program) : (string * string option) list =
  List.filter_map
    (fun (stmt : G.stmt) ->
      match stmt.G.s with
      | G.DefStmt
          ({ G.name = G.EN (G.Id (((name : string), _), _)); _ },
           G.VarDef { G.vinit = (init : G.expr option); _ }) ->
        Some (name, Option.bind init named_function)
      | G.ExprStmt
          ({ G.e =
               G.Assign
                 ({ G.e = G.N (G.Id (((name : string), _), _)); _ }, _,
                  (rhs : G.expr));
             _ }, _) -> Some (name, named_function rhs)
      | _ -> None)
    ast

let function_pointer_aliases (ast : G.program) : (string * string) list =
  let values = file_scope_values ast in
  let bindings_per_name =
    List.fold_left
      (fun (counts : int Common.SMap.t) ((name : string), _) ->
        Common.SMap.update name
          (fun (earlier : int option) ->
            Some (1 + Option.value earlier ~default:0))
          counts)
      Common.SMap.empty values
  in
  List.filter_map
    (fun ((name : string), (target : string option)) ->
      match target with
      | None -> None
      | Some (target : string) ->
        if
          Int.equal
            (Option.value (Common.SMap.find_opt name bindings_per_name)
               ~default:0)
            1
        then Some (name, target)
        else None)
    values

let bind_alias (bound : Func_lookup.scope_entry list Common.SMap.t)
    ((name : string), (target : string))
    : Func_lookup.scope_entry list Common.SMap.t =
  match
    Func_lookup.functions_of_entries
      (Option.value (Common.SMap.find_opt target bound) ~default:[])
  with
  | [] -> bound
  | (_ :: _) as funcs ->
    Common.SMap.add name
      (List.map
         (fun (func : Func_info.t) ->
           { Func_lookup.kind = Func_lookup.Scope_function func;
             parent_path = [] })
         funcs)
      bound

let build
    ~(include_bindings : Scope_binding.positioned_binding list)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    (fi : file_info) : Func_lookup.scope_entry list Common.SMap.t =
  let fi_file_str = Fpath.to_string fi.fi_file in
  let function_bindings =
    Scope_binding.own_definitions_of_file ~file_funcs_index ~fi_file_str
  in
  let alias_bindings =
    Scope_binding.own_alias_bindings ~file_funcs_index ~fi_file_str
  in
  List.fold_left bind_alias
    (Scope_binding.bindings_of_positioned
       (include_bindings @ function_bindings @ alias_bindings))
    (function_pointer_aliases fi.fi_ast)
