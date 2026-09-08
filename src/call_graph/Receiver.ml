(* See the mli. *)

module G = AST_generic

let is_method (fdef : G.function_definition) : bool =
  match fst fdef.G.fkind with
  | G.Method -> true
  | _ -> false

let is_static (entity : G.entity option) : bool =
  match entity with
  | Some ent ->
      List.exists
        (fun (attr : G.attribute) ->
          match attr with
          | G.KeywordAttr (G.Static, _) -> true
          | _ -> false)
        ent.G.attrs
  | None -> false

let implicit_param (lang : Lang.t) ~(is_method : bool) ~(is_static : bool)
    ~(is_first : bool) (param : G.parameter) : bool =
  match param with
  | G.ParamReceiver _ -> true
  | _ -> (
      match lang with
      | Lang.Python | Lang.Python2 | Lang.Python3 ->
          is_first && is_method && not is_static
      | _ -> false)

let arity (lang : Lang.t) ~(is_method : bool) ~(is_static : bool)
    (params : G.parameter list) : int =
  params
  |> List.filteri (fun i param ->
         not (implicit_param lang ~is_method ~is_static ~is_first:(i = 0) param))
  |> List.length

let self_names (lang : Lang.t) : string list =
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3 -> [ "self"; "cls" ]
  | Lang.Php | Lang.Hack -> [ "$this" ]
  | Lang.Ruby | Lang.Crystal | Lang.Rust -> [ "self" ]
  | Lang.Js | Lang.Ts | Lang.Vue | Lang.Java | Lang.Kotlin | Lang.Scala
  | Lang.Csharp | Lang.Dart | Lang.Swift | Lang.Apex | Lang.Cpp | Lang.Solidity ->
      [ "this" ]
  | Lang.Vb -> [ "Me" ]
  | _ -> []

let is_self_name (lang : Lang.t) (name : string) : bool =
  List.exists (String.equal name) (self_names lang)
