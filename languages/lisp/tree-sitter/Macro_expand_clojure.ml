(* Copyright (C) 2026 Opengrep
 *
 * Macroexpand-1 common macros. *)

module G = AST_generic
module GH = AST_generic_helpers

exception Macroexpansion_error of string * G.any

(* This takes precedence, for macroexpansion at translation-to-generic.
 * Second line of patterns must be commented out or disabled otherwise. *)
let expands_as_block (todo_kind : string) =
  match todo_kind  with
  (* When we macroexpand during translation to generic, things like "->"
   * are simply there to tag already-prepared expr blocks. *)
   "MultiLetPatternBindings" | "MultiLetFnBindings" | "ExprBlock"
   | "->" | "->>" | "cond->" | "cond->>"
    -> true
  | _
    -> false

(* XXX: It would be more robust to match on macroexpansion_mode as above. *)
let is_macroexpandable (todo_kind : string) =
  match todo_kind with
  | "as->" | "ShortLambda"
  | "When" | "WhenNot" | "WhenLet" | "WhenSome" | "WhenFirst"
  | "IfLet" | "IfNot" | "IfSome"
    -> true
  | _
    -> false

let macro_expand_1 (expr : G.expr) : G.expr =
  match expr.G.e with

  | G.OtherExpr (("ShortLambda", fn_tok),
                 (G.Params [(G.ParamPattern _pat as param)])
                 :: (G.E expr) :: []) ->
    G.Lambda
      {
        G.fparams = Tok.unsafe_fake_bracket [param];
        frettype = None;
        fkind = (G.LambdaKind, fn_tok);
        fbody = G.FBExpr expr;
      }
    |> G.e

  | G.OtherExpr ((("When" | "WhenNot"), when_tok),
                 (G.E expr_test) :: expr_body) ->
    let do_exprs = match expr_body with
      | [] -> [G.E (L (Null (G.fake "nil")) |> G.e)]
      | _ -> expr_body
    in
    let when_body = 
      G.OtherExpr (("ExprBlock", when_tok), do_exprs)
      |> G.e |> G.exprstmt
    in
    (* TODO: Is it worth to encode the not nil test?
     * TODO: For WhenNot we need an extra NOT( ... ). 
     * Note that an ExprBlock with expr_test :: do_exprs is enough!
     * The exprs will be translated in order. So I'll leave this as
     * is now because it's very easy to improve with the right
     * condition. *)
    G.If (when_tok, (G.Cond expr_test), when_body, None)
    |> G.s |> G.stmt_to_expr

  | G.OtherExpr ((("IfNot"), if_not_tok),
                 G.E expr_test :: G.E then_ :: else_opt) ->
    let else_stmt_opt = match else_opt with
      | [] -> None
      | [ G.E else_ ] -> Some (else_ |> G.exprstmt)
      | _ -> raise @@
        Macroexpansion_error ("Invalid if-not else body", G.E expr)
    in
    let not_expr_test =
      let open G in
      Call (IdSpecial (Op NotEq, if_not_tok) |> G.e,
            Tok.unsafe_fake_bracket
              [ Arg expr_test;
                (* TODO: falsy is nil, false; we only check nil here. *)
                Arg (G.L (G.Null if_not_tok) |> G.e) ]) |> G.e
    in
    G.If (if_not_tok, (G.Cond not_expr_test), then_ |> G.exprstmt, else_stmt_opt)
    |> G.s |> G.stmt_to_expr

  (* TODO: At some point do proper translation:
   * - when-let: when not nil or false;
   * - when-some: when not nil;
   * - when-first: falsy = empty/nil sequence (and binds to first element).
   *   At least we should not bind any name after the first in the PatList.
   *   But this is not enough. *)
  (*
   * (defmacro when-let
   *   "bindings => binding-form test
   * 
   *   When test is true, evaluates body with binding-form bound to the value of test"
   *   {:added "1.0"}
   *   [bindings & body]
   *   (assert-args
   *      (vector? bindings) "a vector for its binding"
   *      (= 2 (count bindings)) "exactly 2 forms in binding vector")
   *    (let [form (bindings 0) tst (bindings 1)]
   *     `(let [temp# ~tst]
   *        (when temp#
   *          (let [~form temp#]
   *            ~@body))))) 
   *  *)
  | G.OtherExpr ((("WhenLet" | "WhenSome" | "WhenFirst"), when_let_tok),
                 G.E { e = G.OtherExpr
                           ((("WhenLetPatternBindings"
                             | "WhenSomePatternBindings"
                             | "WhenFirstPatternBindings"), _when_let_tok),
                            bindings); _}
                 :: expr_body) ->
    let do_exprs = match expr_body with
      | [] -> [G.E (L (Null (G.fake "nil")) |> G.e)]
      | _ -> expr_body
    in
    (* TODO: Again, we could do a proper test to see if the binding
     * is nil. For now this becomes a 'let'. *)
    G.OtherExpr (("ExprBlock", when_let_tok),
                 bindings @ do_exprs)
    |> G.e

  (*
   * if-let: 
   * (let [form (bindings 0) tst (bindings 1)]
   *   `(let [temp# ~tst]
   *      (if temp#
   *        (let [~form temp#]
   *          ~then)
   *        ~else)))))
   * if-some:
   * as above but with 'if not nil? temp#'
   *) 
  | G.OtherExpr
      ((( "IfLet" | "IfSome" ), if_let_tok),
       G.E { e = G.OtherExpr
                 (("ExprBlock", _if_let_tok),
                  G.E {e =
                         G.OtherExpr
                           ((( "IfLetPatternBindings" | "IfSomePatternBindings"),
                             _if_let_tok'),
                            [ G.E G.{e = G.LetPattern (pat_binding, test_expr); _} ] 
                           ); _}
                  :: [ G.E then_ ]); _}
       :: else_opt) ->
    (* We convert to a switch on test_expr: nil -> else | pat -> then.
     * Note that we must have bindings if we are macroexpanding, no ellipsis
     * should reach this point!
     * TODO: Explicit 'truthy' (null for if-some) test, could be better with
     * constant propagation since some cases where the value is known to be nil
     * would be detected; but for now I see marginal value in that. *)
    let if_case = G.case_of_pat_and_expr (pat_binding, then_) in
    let cases =
      begin match else_opt with
        | [] -> [if_case]
        | [ G.E else_ ] ->
          G.case_of_pat_and_expr
            (G.PatLiteral (G.Null (G.fake "nil")), else_) :: if_case :: []
        | _ -> raise @@ 
          Macroexpansion_error ("Invalid if-let else body", G.E expr)
      end
    in
    G.Switch
      (if_let_tok,
       Some (G.Cond test_expr),
       cases)
    |> G.s |> G.stmt_to_expr

  (*
   * (defmacro as->
   *   "Binds name to expr, evaluates the first form in the lexical context
   *   of that binding, then binds name to that result, repeating for each
   *   successive form, returning the result of the last form."
   *   {:added "1.5"}
   *   [expr name & forms]
   *   `(let [~name ~expr
   *          ~@(interleave (repeat name) (butlast forms))]
   *      ~(if (empty? forms)
   *         name
   *         (last forms)))) 
   *  *)
  | G.OtherExpr (("as->", thread_tk),
                 G.E v_expr :: G.P orig_binding ::
                 interleaved_expr_pat) ->
    let rec comp_as_body = function
      | G.E {e = G.Ellipsis _; _} as expr :: rest_exprs ->
        expr :: comp_as_body rest_exprs
      | G.E expr :: G.P pat :: rest_exprs ->
        G.E {expr with e = G.(LetPattern (pat, expr))}
        :: comp_as_body rest_exprs
      | [ G.E _ as expr ] -> [ expr ]
      | other :: _ -> raise @@
        Macroexpansion_error ("Invalid as-> body.", other)
      | _ -> raise @@
        Macroexpansion_error ("Invalid as-> body.", Tk thread_tk)
    in
    begin match interleaved_expr_pat with
    | [] -> v_expr (* no need for binding at all, or for GH.pattern_to_expr. *)
    | _ ->
      let exs_any =
        G.E (G.(LetPattern (orig_binding, v_expr) |> e))
        :: comp_as_body interleaved_expr_pat
      in
      G.OtherExpr (("ExprBlock", thread_tk), exs_any)
      |> G.e
    end

  | _ ->
    raise @@ Macroexpansion_error ("Not implemented", G.E expr)

