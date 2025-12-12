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
(* Elixir: ShortLambda / Capture operator *)
(* ========================================================================== *)

(** Elixir: Convert ShortLambda to a regular Lambda.

    ShortLambda comes from Elixir_to_generic as:
    OtherExpr("ShortLambda", [Params params; S body_stmt])

    This converts it to a proper Lambda for IL/taint analysis. *)
let convert_elixir_short_lambda (e : G.expr) : G.expr =
  match e.G.e with
  | G.OtherExpr (("ShortLambda", tok), [ G.Params params; G.S body ]) ->
      let fdef =
        {
          G.fparams = Tok.unsafe_fake_bracket params;
          frettype = None;
          fkind = (G.LambdaKind, tok);
          fbody = G.FBStmt body;
        }
      in
      G.Lambda fdef |> G.e
  | _ -> e
