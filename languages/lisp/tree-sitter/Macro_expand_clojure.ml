(** Macroexpand-1 common macros like ->, ->>, if-let, if-not for tainting.
 ** Used in AST_to_IL.
 **
 ** Disclaimer:
 ** Nothing that affects bindings should happen here, because this expansion
 ** happens after parsing, for IL only. This is because we need the original
 ** source for patterns, including in tainting mode.
 ** In practice this is good enough. 
 **  
 ** Macros:
 **  
 ** ->
 ** ->> 
 ** as->    
 ** cond->
 ** cond->>
 ** some->   
 ** some->>  
 ** if-let   
 ** if-not
 ** if-some 
 ** when
 ** when-let 
 ** when-not 
 **  
 **  *)

(* open Common *)
module G = AST_generic
module GH = AST_generic_helpers
module S = Settings_clojure

type insert_pos = 
  | Insert_first
  | Insert_last

exception Macroexpansion_error of string * G.any

(* This takes precedence, for macroexpansion at translation-to-generic.
 * Second line of patterns must be commented out or disabled otherwise. *)
let expands_as_block (todo_kind : string) =
  match S.macroexpansion_mode, todo_kind  with
  | S.At_IL, ("MultiLetPatternBindings" | "MultiLetFnBindings" | "ExprBlock")
    -> true
  | S.At_generic,
    (* When we macroexpand during translation to generic, things like "->"
     * are simply there to tag already-prepared expr blocks. *)
    ("MultiLetPatternBindings" | "MultiLetFnBindings" | "ExprBlock"
    | "->" | "->>" | "cond->" | "cond->>")
    -> true
  | _
    -> false

(* XXX: It would be more robust to match on macroexpansion_mode as above. *)
let is_macroexpandable (todo_kind : string) =
  match todo_kind with
  | "->" | "->>" | "cond->" | "cond->>" | "as->" | "ShortLambda"
  | "When" | "WhenNot" | "WhenLet" | "WhenSome" | "WhenFirst"
  | "IfLet" | "IfNot" | "IfSome"
    -> true
  | _
    -> false

(* 
 * This is also valid:
 *  
 * - (-> [x 1] (let (+ 1 x)))   => 2
 * - ((-> [x] (fn (+ 1 x))) 1)  => 2
 *  
 * Not implemented and will fail to parse because the placeholder
 * is not a valid binding form. We could simply expand once here
 * for such cases... but it's simply not worth the effort.
 *
 * In theory we could allow that by looking at the first form
 * in which we will thread the value in, for -> only, and in that
 * case decide to treat as bindings.
 *)
let insert_threaded insert_pos v e =

  let insert_last x xs = List.rev (x :: (List.rev xs)) in

  match e.G.e with

  (* NOTE: If macroexpanding raw, wrap in `List with no meta and fake parens. *)
  | G.N _ | G.L _ | G.Container _ | G.Record _
  | G.OtherExpr (("Atom", _), [G.Name _]) ->
    (* TODO: Special cases for atom etc which means G.DotAccess? *)
    {e with e = G.Call (e, Tok.unsafe_fake_bracket [G.Arg v])}

  | G.Call (expr, (lp, args, rp)) ->
    let new_arg = G.Arg v in
    let args' = match insert_pos with
      | Insert_first -> new_arg :: args
      | Insert_last -> insert_last new_arg args
    in
    {e with e = G.Call (expr, (lp, args', rp))}

  (* FIXME (S.At_IL): Deal with as->, for example:
   * (-> 5 (as-> x (+ x x) (+ x x)) )
   * works. *)
  | G.OtherExpr (((( "->" | "->>" | "as->" | "ExprBlock"), _tk) as td_kind), exprs) ->
    (* FIXME: letfn and let create ExprBlock, we should not process these
     * for ->, but only for ->>. *)
    let new_exp = G.E v in
    let exprs' = match insert_pos with
      | Insert_first -> new_exp :: exprs
      | Insert_last -> insert_last new_exp exprs
    in
    {e with e = G.OtherExpr (td_kind, exprs')}

  | G.OtherExpr (("PartialIf", if_tk), if_exprs) ->
    begin match insert_pos with
    | Insert_first ->
      begin match if_exprs with
      | [G.E if_expr] ->
        G.If (if_tk, (G.Cond v),
              if_expr |> G.exprstmt,
              None) |> G.s |> G.stmt_to_expr
      | [G.E if_expr; G.E then_expr ] ->
        G.If (if_tk, (G.Cond v),
              if_expr |> G.exprstmt,
              Some (then_expr |> G.exprstmt))
        |> G.s |> G.stmt_to_expr
      | _ -> assert false end
    | Insert_last ->
      begin match if_exprs with
      | [G.E test_expr] ->
        G.If (if_tk, (G.Cond test_expr),
              v |> G.exprstmt,
              None) |> G.s |> G.stmt_to_expr
      | [G.E test_expr; G.E if_expr ] ->
        G.If (if_tk, (G.Cond test_expr),
              if_expr |> G.exprstmt,
              Some (v |> G.exprstmt))
        |> G.s |> G.stmt_to_expr
      | _ -> assert false end
    end

  (* FIXME: This does not happen, we use ExprBlock in let now (should revert).
   * But it would be ok for ->>, just not for ->. Same for the lambda case
   * below. *)
  | G.OtherExpr (("MultiLetPatternBlock", _), _) ->
    (* (->> (print x) (let [x 2] (+ 10 x))) => prints 2.
     * We won't bother with this, it requires too much mechanics
     * and don't expect to see this in the wild.
     * The only reasonable way to capture these things is to
     * macroexpand in parsing, but then everything else will break. *)
    raise @@
    Macroexpansion_error ("Threading binding forms is not implemented.",
                          G.E e)
  | G.Lambda _ ->
    (* Actually ((->> 4 (fn [x])) 1) => 4 but is there a sane person
     * that does that? *)
    raise @@
    Macroexpansion_error ("Threading binding forms is not implemented.",
                          G.E e)
  | _ ->
    (* TODO: Log this... *)
    (* TODO: Should this be an exception vs assert? We have more constructs now, this
     * should not be assert any more. *)
    ignore ( assert false );
    e

let macro_expand_1 (expr : G.expr) : G.expr =
  match expr.G.e with

  (* XXX: No effect if macroexpansion happens on generic translation. *)
  | G.OtherExpr (((( "->" | "->>") as first_or_last), _tk),
                 (G.E v_expr) :: rest_exprs) ->
    let unpacked_rest_exprs =
      List.map (function
          | G.E expr -> expr
          | _ -> failwith "Expected expr") rest_exprs
    in
    let pos =
      match first_or_last with
      | "->" -> Insert_first
      | _ (* ->> *) -> Insert_last
    in
    List.fold_left (insert_threaded pos) v_expr unpacked_rest_exprs

  (*
   * (let [g (gensym)
   *         steps (map (fn [[test step]] `(if ~test (-> ~g ~step) ~g))
   *                    (partition 2 claues))]
   *     `(let [~g ~expr
   *            ~@(interleave (repeat g) (butlast steps))]
   *        ~(if (empty? steps)
   *           g
   *           (last steps)))))
   *
   * opengrep.clj=> (macroexpand-1 '(cond-> 1 true (+ 1) false (- 1)))
   * replacing a gensym such as G__2174 with g: 
   * (let [g 1
   *       g (if true (-> g (+ 1)) g)]
   *  (if false (-> g (- 1)) g)) 
   *)
  | G.OtherExpr (((( "cond->" | "cond->>" ) as cond_first_or_last), _tk),
                 (G.E v_expr) :: tests_and_exprs) ->
    let make_tmp ?sid () =
      let open Common in
      let sid = sid ||| G.SId.mk () in
      let name_kind = G.LocalVar in
      let entname = (name_kind, sid) in
      let temp_ident =
        (* Can this shadow uses in functions? Don't think so.
         * But how about nested cond-> ? Add SId suffix? *)
        (G.implicit_param, G.fake "cond-threaded-temp" (* first_or_last? *))
      in
      let id_info = G.basic_id_info ~hidden:true entname in
      G.PatId (temp_ident, id_info)
    in
    let make_if_thread_else previous_ident pos test_expr body_expr =
      let ident_expr =
        G.N previous_ident |> G.e
      in
      (* TODO: Make condition more specific, to work with
       * constant propagation (will it work)? *)
      G.If (G.fake "if-cond", (G.Cond test_expr),
            insert_threaded pos ident_expr body_expr |> G.exprstmt,
            Some (ident_expr |> G.exprstmt))
      |> G.s |> G.stmt_to_expr
    in
    let make_let new_pat expr = 
      G.LetPattern (new_pat, expr) |> G.e
    in
    begin match tests_and_exprs with
    | [] -> v_expr (* no need for binding at all *)
    | _ ->
      let last_expr, last_test, rest_tests_exprs_anys =
        match List.rev tests_and_exprs with
        | G.E last_expr :: G.E last_test :: rest_rev ->
          (last_expr, last_test, List.rev rest_rev)
        | _ -> assert false
      in
      let init_pat_id = make_tmp () in
      let init_exr = 
        make_let init_pat_id v_expr
      in
      let unpacked_rest_tests_exprs =
        List.map (function
            | G.E expr -> expr
            (* TODO: raise proper exception: *)
            | _ -> failwith "Expected expr") rest_tests_exprs_anys
      in
      let pos =
        match cond_first_or_last with
        | "cond->" -> Insert_first
        | _ (* cond->> *) -> Insert_last
      in
      let test_expr_pairs =
        let rec aux acc = function
          | test_expr :: body_expr :: rest ->
            aux ((test_expr, body_expr) :: acc) rest
          | [] -> List.rev acc
          (* TODO: raise proper exception: *)
          | _ -> failwith "Expected even number of test and body exprs"
        in
        aux [] unpacked_rest_tests_exprs
      in
      let last_binding, intermediate_binding_exprs =
        List.fold_left_map
          (fun last_binding (test_expr, body_expr) ->
             begin match last_binding with
            | G.PatId (prev_ident, id_info) ->
                let new_tmp = make_tmp () in
                let if_expr =
                    make_if_thread_else
                      (G.Id (prev_ident, id_info))
                      pos
                      test_expr
                      body_expr
                in
                (new_tmp, (make_let new_tmp if_expr))
            | _ -> assert false
             end
          )
          init_pat_id
          test_expr_pairs
      in
      let last_id = match last_binding with
        | G.PatId (last_ident, id_info) -> G.Id (last_ident, id_info)
        | _ -> assert false
      in
      let last_if_expr = 
        make_if_thread_else last_id pos last_test last_expr
      in
      let body_exprs =
        List_.map (fun e -> G.E e)
          (init_exr :: intermediate_binding_exprs @ [last_if_expr])
      in
      G.OtherExpr (("ExprBlock", G.fake "cond-threaded-block"), body_exprs)
      |> G.e
    end

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

