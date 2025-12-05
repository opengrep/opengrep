(** AST modifications/normalizations applied before IL conversion.

    These transformations normalize language-specific AST patterns into
    more canonical forms that are easier to analyze in IL/taint analysis.
*)

open Common
module G = AST_generic

(* ========================================================================== *)
(* Lua: Array-like tables *)
(* ========================================================================== *)

(** Lua: Check if a Dict container is actually an array-like table.
    Lua tables like {1, 2, 3} are parsed as Dict with NextArrayIndex keys,
    but should be treated as arrays for taint analysis. *)
let is_lua_array_table (lang : Lang.t) (entries : G.expr list) : bool =
  lang =*= Lang.Lua
  && List.for_all
       (fun entry ->
         match entry.G.e with
         | G.Assign ({ e = G.IdSpecial (G.NextArrayIndex, _); _ }, _, _) -> true
         | _ -> false)
       entries

(** Lua: Extract values from array-like table entries.
    Converts Assign(NextArrayIndex, value) entries to just the values. *)
let extract_lua_array_values (entries : G.expr list) : G.expr list =
  entries
  |> List.map (fun entry ->
         match entry.G.e with
         | G.Assign (_, _, value) -> value
         | _ -> assert false)

(* ========================================================================== *)
(* Elixir: Implicit parameter lambdas *)
(* ========================================================================== *)

(** Elixir: Convert implicit parameter lambda to explicit parameter lambda.

    Elixir lambdas like `fn x -> sink(x) end` are parsed with an implicit
    parameter and a Switch body. This converts them to explicit parameter
    lambdas for proper taint tracking. *)
let convert_elixir_implicit_lambda (fdef : G.function_definition)
    : G.function_definition =
  match (Tok.unbracket fdef.G.fparams, fdef.G.fbody) with
  | ( [ G.Param { pname = Some _ ; _ } ],
      G.FBStmt { s = G.Switch (_, Some (G.Cond _), cases); _ } )
    when  List.length cases =*= 1 -> (
      match cases with
      | [
       G.CasesAndBody
         ( [
             G.Case
               ( _,
                 G.OtherPat
                   ( ("ArgsAndWhenOpt", _),
                     [ G.Args [ G.Arg { e = G.N (G.Id (id, _)); _ } ] ] ) );
           ],
           body_stmt );
      ] ->
          (* Convert to lambda with explicit parameter from the pattern *)
          let new_param = G.Param (G.param_of_id id) in
          {
            fdef with
            fparams = Tok.unsafe_fake_bracket [ new_param ];
            fbody = G.FBStmt body_stmt;
          }
      | _ -> fdef)
  | _ -> fdef

(* ========================================================================== *)
(* Elixir: ShortLambda / Capture operator *)
(* ========================================================================== *)

(** Elixir: Find the maximum placeholder number in a ShortLambda body.
    E.g., in &(foo(&1, &3)), returns 3. *)
let rec find_max_placeholder (e : G.expr) : int =
  match e.G.e with
  | G.OtherExpr
      (("PlaceHolder", _), [ G.E { e = G.L (G.Int (Some n, _)); _ } ]) ->
      Int64.to_int n
  | G.Call (callee, (_, args, _)) ->
      let callee_max = find_max_placeholder callee in
      let args_max =
        args
        |> List.fold_left
             (fun acc arg ->
               match arg with
               | G.Arg e | G.ArgKwd (_, e) -> max acc (find_max_placeholder e)
               | _ -> acc)
             0
      in
      max callee_max args_max
  | _ -> 0

(** Elixir: Replace placeholder references with parameter names.
    E.g., &1 becomes x0, &2 becomes x1, etc. *)
let rec replace_placeholders (e : G.expr) : G.expr =
  match e.G.e with
  | G.OtherExpr (("PlaceHolder", _), [ G.E { e = G.L (G.Int (Some n, tk)); _ } ])
    ->
      let param_idx = Int64.to_int n - 1 in
      let param_name = Printf.sprintf "x%d" param_idx in
      let param_id = (param_name, tk) in
      G.N (G.Id (param_id, G.empty_id_info ())) |> G.e
  | G.Call (callee, (lp, args, rp)) ->
      let new_callee = replace_placeholders callee in
      let new_args =
        args
        |> List.map (fun arg ->
               match arg with
               | G.Arg e -> G.Arg (replace_placeholders e)
               | G.ArgKwd (id, e) -> G.ArgKwd (id, replace_placeholders e)
               | other -> other)
      in
      G.Call (new_callee, (lp, new_args, rp)) |> G.e
  | _ -> e

(** Elixir: Convert ShortLambda/Capture (&expression) to a regular Lambda.

    Elixir's capture operator & creates anonymous functions:
    - &foo/1 captures a function reference (converted at parse time)
    - &(sink(&1)) creates a lambda where &1 is the first parameter

    This converts captures with placeholders to regular lambdas for taint analysis. *)
let convert_elixir_short_lambda (tok : Tok.t) (body_expr : G.expr) : G.expr =
  let max_placeholder = find_max_placeholder body_expr in
  let replaced_body = replace_placeholders body_expr in
  let params =
    List.init max_placeholder (fun i ->
        let param_name = Printf.sprintf "x%d" i in
        let param_id = (param_name, tok) in
        G.Param (G.param_of_id param_id))
  in
  let body_stmt = G.ExprStmt (replaced_body, G.sc) |> G.s in
  let fdef =
    {
      G.fparams = Tok.unsafe_fake_bracket params;
      frettype = None;
      fkind = (G.LambdaKind, tok);
      fbody = G.FBStmt body_stmt;
    }
  in
  G.Lambda fdef |> G.e
