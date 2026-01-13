(* Iago Abal
 *
 * Copyright (C) 2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

open Common
module Log = Log_tainting.Log
module G = AST_generic
module T = Taint
module Taints = T.Taint_set
module R = Rule
open Shape_and_sig.Shape
module Fields = Shape_and_sig.Fields
module Shape = Taint_shape
module Effect = Shape_and_sig.Effect
module Effects = Shape_and_sig.Effects
module Signature = Shape_and_sig.Signature
module Lval_env = Taint_lval_env

let sigs_tag = Log_tainting.sigs_tag
let bad_tag = Log_tainting.bad_tag

(*****************************************************************************)
(* Call effets *)
(*****************************************************************************)

type call_effect =
  | ToSink of Effect.taints_to_sink
  | ToReturn of Effect.taints_to_return
  | ToLval of Taint.taints * IL.name * Taint.offset list
  | ToSinkInCall of {
      callee : IL.exp;
      arg : Taint.arg;
      args_taints : Effect.args_taints;
    }

type call_effects = call_effect list

let show_call_effect = function
  | ToSink tts -> Effect.show_taints_to_sink tts
  | ToReturn ttr -> Effect.show_taints_to_return ttr
  | ToLval (taints, var, offset) ->
      Printf.sprintf "%s ----> %s%s" (T.show_taints taints) (IL.str_of_name var)
        (T.show_offset_list offset)
  | ToSinkInCall { callee; arg; _ } ->
      Printf.sprintf "ToSinkInCall(%s, %s)" (Display_IL.string_of_exp callee)
        (T.show_arg arg)

let show_call_effects call_effects =
  call_effects |> List_.map show_call_effect |> String.concat "; "

(*****************************************************************************)
(* Instantiation "config" *)
(*****************************************************************************)

type inst_var = {
  inst_lval : T.lval -> (Taints.t * shape) option;
      (** How to instantiate a 'Taint.lval', aka "data taint variable". *)
  inst_ctrl : unit -> Taints.t;
      (** How to instantiate a 'Taint.Control', aka "control taint variable". *)
}

(* TODO: Right now this is only for source traces, not for sink traces...
 * In fact, we should probably not have two traces but just one, but more
 * general. *)
type inst_trace = {
  add_call_to_trace_for_src :
    Tok.t list ->
    Rule.taint_source T.call_trace ->
    Rule.taint_source T.call_trace option;
      (** For sources we extend the call trace. *)
  fix_token_trace_for_var : var_tokens:Tok.t list -> Tok.t list -> Tok.t list;
      (** For variables we should too, but due to limitations in our call-trace
          * representation, we just record the path as tainted tokens. *)
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ( let+ ) x f =
  match x with
  | None -> []
  | Some x -> f x

(*****************************************************************************)
(* Instantiating traces *)
(*****************************************************************************)

(* Try to get an idnetifier from a callee/function expression, to be used in
 * a taint trace. *)
let get_ident_of_callee callee =
  match callee with
  | { IL.e = Fetch f; eorig = _ } -> (
      match f with
      (* Case `f()` *)
      | { base = Var { ident; _ }; rev_offset = []; _ }
      (* Case `obj. ... .m()` *)
      | { base = _; rev_offset = { o = Dot { ident; _ }; _ } :: _; _ } ->
          Some ident
      | __else__ -> None)
  | __else__ -> None

let add_call_to_trace_if_callee_has_eorig ~callee tainted_tokens call_trace =
  (* E.g. (ToReturn) the call to 'bar' in:
   *
   *     1 def bar():
   *     2     x = taint
   *     3     return x
   *     4
   *     5 def foo():
   *     6     y = bar()
   *     7     sink(y)
   *
   * would result in this call trace for the source:
   *
   *     Call('bar' @l.6, ["x" @l.2], "taint" @l.2)
   *
   * E.g. (ToLval) the call to 'bar' in:
   *
   *     1 s = set([])
   *     2
   *     3 def bar():
   *     4    global s
   *     5    s.add(taint)
   *     6
   *     7 def foo():
   *     8    global s
   *     9    bar()
   *    10    sink(s)
   *
   * would result in this call trace for the source:
   *
   *     Call('bar' @l.6, ["s" @l.5], "taint" @l.5)
   *)
  match callee with
  | { IL.e = _; eorig = SameAs orig_callee } ->
      Some (T.Call (orig_callee, tainted_tokens, call_trace))
  | __else__ ->
      (* TODO: Have a better fallback in case we can't get an eorig from 'callee',
       * maybe for that we need to change `Taint.Call` to accept a token. *)
      None

let add_call_to_token_trace ~callee ~var_tokens caller_tokens =
  (* E.g. (ToReturn) the call to 'bar' in:
   *
   *     1 def bar(x):
   *     2     y = x
   *     3     return y
   *     4
   *     5 def foo():
   *     6     t = bar(taint)
   *     7     ...
   *
   * would result in this list of tokens (note that is reversed):
   *
   *     ["t" @l.6; "y" @l.2; "x" @l.1; "bar" @l.6]
   *
   * This is a hack we use because taint traces aren't general enough,
   * this should be represented with a call trace.
   *)
  let call_tokens =
    (match get_ident_of_callee callee with
    | None -> []
    | Some ident -> [ snd ident ])
    @ List.rev var_tokens
  in
  List.rev_append call_tokens caller_tokens

let add_lval_update_to_token_trace ~callee:_TODO lval_tok ~var_tokens
    caller_tokens =
  (* E.g. (ToLval) the call to 'bar' in:
   *
   *     1 s = set([])
   *     2
   *     3 def bar(x):
   *     4    global s
   *     5    s.add(x)
   *     6
   *     7 def foo():
   *     8    global s
   *     9    t = taint
   *    10    bar(t)
   *    11    sink(s)
   *
   * would result in this list of tokens (note that is reversed):
   *
   *     ["s" @l.5; "s" @l.5; "x" @l.3; "s" @l.5; "bar" @l.10; "t" @l.9]
   *
   * This is a hack we use because taint traces aren't general enough,
   * this should be represented with a call trace.
   *)
  let call_tokens =
    (* TODO: Use `get_ident_of_callee callee` to add the callee to the trace. *)
    lval_tok :: List.rev var_tokens
  in
  List.rev_append call_tokens caller_tokens

(*****************************************************************************)
(* Instatiation *)
(*****************************************************************************)

let subst_in_precondition inst_var taint =
  let subst taints =
    taints
    |> List.concat_map (fun t ->
           match t.T.orig with
           | Src _ -> [ t ]
           | Var lval -> (
               match inst_var.inst_lval lval with
               | None -> []
               | Some (call_taints, _call_shape) ->
                   call_taints |> Taints.elements)
           | Shape_var lval -> (
               match inst_var.inst_lval lval with
               | None -> []
               | Some (_call_taints, call_shape) ->
                   (* Taint shape-variable, stands for the taints reachable
                    * through the shape of the 'lval', it's like a delayed
                    * call to 'Shape.gather_all_taints_in_shape'. *)
                   Shape.gather_all_taints_in_shape call_shape
                   |> Taints.elements)
           | Control -> inst_var.inst_ctrl () |> Taints.elements)
  in
  T.map_preconditions subst taint

let instantiate_taint_var inst_var taint =
  match taint.T.orig with
  | Src _ -> None
  | Var lval -> inst_var.inst_lval lval
  | Shape_var lval ->
      (* This is just a delayed 'gather_all_taints_in_shape'. *)
      let* taints =
        inst_var.inst_lval lval
        |> Option.map (fun (_taints, shape) ->
               Shape.gather_all_taints_in_shape shape)
      in
      Some (taints, Bot)
  | Control ->
      (* 'Control' is pretty much like a taint variable so we handle all together. *)
      Some (inst_var.inst_ctrl (), Bot)

let instantiate_taint inst_var inst_trace taint =
  let inst_taint_var taint = instantiate_taint_var inst_var taint in
  match taint.T.orig with
  | Src src -> (
      let taint =
        match
          inst_trace.add_call_to_trace_for_src taint.tokens src.call_trace
        with
        | Some call_trace ->
            { T.orig = Src { src with call_trace }; tokens = [] }
        | None -> taint
      in
      match subst_in_precondition inst_var taint with
      | None ->
          (* substitution made preconditon false, so no taint here! *)
          Taints.empty
      | Some taint -> Taints.singleton taint)
  (* Taint variables *)
  | Var _
  | Shape_var _
  | Control -> (
      match inst_taint_var taint with
      | None -> Taints.empty
      | Some (call_taints, _Bot_shape) ->
          call_taints
          |> Taints.map (fun taint' ->
                 {
                   taint' with
                   tokens =
                     inst_trace.fix_token_trace_for_var ~var_tokens:taint.tokens
                       taint'.tokens;
                 }))

let instantiate_taints inst_var inst_trace taints =
  taints |> Taints.to_seq
  |> Seq.fold_left
       (fun acc taint ->
         acc |> Taints.union (instantiate_taint inst_var inst_trace taint))
       Taints.empty

let instantiate_shape inst_var inst_trace shape =
  let inst_taints = instantiate_taints inst_var inst_trace in
  let rec inst_shape = function
    | Bot -> Bot
    | Obj obj ->
        let obj =
          obj
          |> Fields.filter_map (fun _o cell ->
                 (* This is essentially a recursive call to 'instantiate_shape'!
                  * We rely on 'update_offset_in_cell' to maintain INVARIANT(cell). *)
                 Shape.update_offset_in_cell ~f:inst_xtaint [] cell)
        in
        if Fields.is_empty obj then Bot else Obj obj
    | Arg arg -> (
        match inst_var.inst_lval (T.lval_of_arg arg) with
        | Some (_taints, shape) -> shape
        | None ->
            Log.warn (fun m ->
                m "Could not instantiate arg shape: %s" (T.show_arg arg));
            Arg arg)
    | Fun _ as funTODO ->
        (* Right now a function shape can only come from a top-level function,
         * whose shape will not depend on the parameters of another encloding
         * function, so we shouldn't have to instantiate anything here, e.g.:
         *
         *     def bar():
         *       ...
         *
         *     def foo(x):
         *       return bar
         *
         * When instantiating a call like `foo(1)`, the shape of `bar` (that is,
         * its signature) in `return bar` does not depend on `x` at all. (If the
         * function is applied, then its signature will be instantiated as usual.)
         *
         * This will change when we start giving taint signatures to lambdas,
         * as they can capture variables from their enclosing function, so when
         * instantiating the enclosing function we also need to instantiate the
         * shape of the lambda, e.g.:
         *
         *     def foo(x):
         *       return (lambda y: x)
         *
         *)
        funTODO
  and inst_xtaint xtaint shape =
    (* This may break INVARIANT(cell) but 'update_offset_in_cell' will restore it. *)
    let xtaint =
      match xtaint with
      | `None
      | `Clean ->
          xtaint
      | `Tainted taints -> `Tainted (inst_taints taints)
    in
    let shape = inst_shape shape in
    (xtaint, shape)
  in
  inst_shape shape

(* NOTE: 'a is either:
 * - IL.exp in instantiate_lval_using_actual_exps
 * - Taints.t * shape in instantiate_lval_using_shape *)
let find_pos_in_actual_args ?(err_ctx = "???") (args : 'a IL.argument list)
    (fparams : Signature.params) ~(combine_rest_args : 'a list -> 'a) : T.arg -> 'a option =
  Log.debug (fun m ->
      m "FIND_POS_IN_ACTUAL_ARGS: err_ctx=%s, num_args=%d, num_fparams=%d, fparams=%s"
        err_ctx
        (List.length args)
        (List.length fparams)
        (fparams |> List.map Signature.show_param |> String.concat ", "));
  (* We go left-to-right through formal params. If a param is named and there
   * is an actual named arg, use it; if not, take the first non-named actual
   * arg available. NOTE that it is the Python semantics, and can potentially lead 
   * to problems with languages like OCaml, where there is a clear distinction
   * between named and non-named arguments. *)
  let pos_args, named_args =
    args |>
    List.partition_map (function
      | IL.Unnamed v -> Left v
      | IL.Named ((name, _token), v) -> Right (name, v))
  in
  let formal_args_with_vals =
    fparams |>
    List.map (function
       | (Signature.P name as p)
       | (Signature.PRest name as p) -> Some (p, List.assoc_opt name named_args)
       | _ -> None)
  in
  let rec merge formal_args_with_vals pos_args =
     match formal_args_with_vals, pos_args with
     (* No more formal args, no more actual args: we're done *)
     | [], [] -> []
     (* No more formal args, but there are still positional args *)
     | [], _ ->
        Log.err (fun m ->
          m "function applied to more arguments than expected by the signature (%s)" err_ctx);
          []
     (* The formal arg doesn't get a value (not found among named args, 
      * and no more positional args) *)
     | None :: _ , []
     | Some (Signature.P _, None) :: _, [] ->
        Log.err (fun m ->
          m "function applied to fewer arguments than expected by the signature (%s)" err_ctx);
          []
     (* The value for the formal arg is found among named actual args *)
     | Some (Signature.P name, Some v) :: avs, _
     | Some (Signature.PRest name, Some v) :: avs, _ (* possible? *) ->
        (Some name, v) :: merge avs pos_args
     (* Not found among named actual args, so we assign the first 
      * available positional arg *)
     | Some (Signature.P name, None) :: name_vals, v :: pos_args ->
        (Some name, v) :: merge name_vals pos_args
     (* The rest argument takes all positional args *)
     | Some (Signature.PRest name, None) :: name_vals, _ ->
        (Some name, combine_rest_args pos_args) :: merge name_vals []
     (* The formal arg does not have a name *)
     | None :: name_vals, v :: pos_args ->
         (None, v) :: merge name_vals pos_args
     | Some (Signature.Other, _) :: _, _ ->
         raise Impossible
  in
  let name_opt_value_list = merge formal_args_with_vals pos_args in
  let param_index_array = Array.of_list (List.map snd name_opt_value_list) in
  let param_name_map =
    name_opt_value_list
    |> List.filter_map
         (fun (a, b) -> Option.map (fun a -> (a, b)) a)
    |> SMap.of_list
  in
  (* lookup function *)
  fun ({ name = s; index = i } : Taint.arg) ->
    match SMap.find_opt s param_name_map with
    | Some _ as r -> r
    | _ when i < 0 || i >= Array.length param_index_array ->
        Log.debug (fun m ->
          (* TODO: provide more context for debugging *)
          m ~tags:bad_tag
            "Cannot match taint variable with function arguments (%i: %s)" i s);
        None
    | _ -> Some (Array.get param_index_array i)

(* Test find_pos_in_actual_args.
 * Function: foo(x, y, _, z)
 * Call: foo(0, x=1, 2, 3) 
 * Expected: x -> 1, y -> 0, _ -> 2, z -> 3 *)
let%test _ =
  let named s v = IL.Named ((s, G.fake ""), v) in
  let params = Signature.([P "x"; P "y"; Other; P "z"]) in
  let args = IL.([Unnamed 0; named "x" 1; Unnamed 2; Unnamed 3]) in
  let func = find_pos_in_actual_args args params ~combine_rest_args:List.hd in
  let open T in
  Option.equal (=|=) (func {name = "x"; index = -1}) (Some 1) &&
  Option.equal (=|=) (func {name = "y"; index = -1}) (Some 0) &&
  Option.equal (=|=) (func {name = "z"; index = -1}) (Some 3) &&
  Option.equal (=|=) (func {name = "";  index = 0})  (Some 1) &&
  Option.equal (=|=) (func {name = "";  index = 1})  (Some 0) &&
  Option.equal (=|=) (func {name = "";  index = 2})  (Some 2) &&
  Option.equal (=|=) (func {name = "";  index = 3})  (Some 3)

let combine_rest_args_exp (es : IL.exp list) : IL.exp =
  let e = IL.Composite (IL.CList, Tok.unsafe_fake_bracket es) in
  let eorig =
    es
    |> List.map (fun x -> IL.any_of_orig (x.IL.eorig))
    |> (fun x -> IL.Related (G.Anys x))
  in
  {e; eorig}

(* Given a function/method call 'fun_exp'('args_exps'), and a taint variable 'tlval'
    from the taint signature of the called function/method 'fun_exp', we want to
   determine the actual l-value that corresponds to 'lval' in the caller's context.

    The return value is a triplet '(variable, offset, token)', where 'token' is to
    be added to the taint trace, and it may even be the token of 'variable'.
    For example, if we are calling `obj.method` and `this.x` were tainted, then we
    would record that taint went through `obj`.

    TODO(shapes): This is needed for stuff that is not yet fully adapted to shapes,
             in theory we should only need 'instantiate_lval_using_shape'.
*)
let instantiate_lval_using_actual_exps (fun_exp : IL.exp) fparams args_exps
    (tlval : T.lval) : (IL.name * T.offset list * T.tainted_token) option =
  (* Error handling  *)
  let log_error () =
    Log.err (fun m ->
        m "instantiate_lval_using_actual_exps FAILED: %s(...): %s"
          (Display_IL.string_of_exp fun_exp)
          (T.show_lval tlval))
  in
  let ( let* ) opt f =
    match opt with
    | None -> None
    | Some x -> (
        match f x with
        | None ->
            log_error ();
            None
        | Some r -> Some r)
  in
  match tlval.base with
  | BGlob gvar -> Some (gvar, tlval.offset, snd gvar.ident)
  | BArg pos -> (
      (*
          An actual argument from 'args_exps', e.g.

              instantiate_lval_using_actual_exps f [x;y;z] [a.q;b;c]
                                { base = BArg {name = "x"; index = 0}; offset = [.u] }
              = (a, [.q.u], tok)
        *)
      let* (arg_exp : IL.exp) =
        find_pos_in_actual_args
          ~err_ctx:(Display_IL.string_of_exp fun_exp)
          ~combine_rest_args:combine_rest_args_exp
          args_exps fparams pos
      in
      match (arg_exp.e, tlval.offset) with
      | Fetch ({ base = Var obj; _ } as arg_lval), _ ->
          let* var, offset = Lval_env.normalize_lval arg_lval in
          Some (var, offset @ tlval.offset, snd obj.ident)
      | __else__ -> None)
  | BThis -> (
      (*
          A field of the callee object, e.g.:

              instantiate_lval_using_actual_exps o.f [] []
                                { base = BThis; offset = [.x] }
              = (o, [.x], tok)

          For the call trace, we try to record variables that correspond to objects,
          but if not possible then we record method names.
        *)
      match fun_exp with
      | { e = Fetch { base = Var method_; rev_offset = [] }; _ }
      (* fun_exp = `method(...)` *) -> (
          (* lval = `this.x.y.z` so we assume to be calling a class method, and
             because the call `method(...)` has no explicit receiver object, then
             we assume it is the `this` object in the caller's context. Thus,
             the instantiated l-vale is `x.y.z`. *)
          match tlval.offset with
          | Ofld var :: offset -> Some (var, offset, snd method_.ident)
          | []
          | (Oint _ | Ostr _ | Oany) :: _ ->
              (* we have no 'var' to take here *)
              log_error ();
              None)
      | {
       (* fun_exp = `<base>. ... .method(...)` *)
       e = Fetch { base; rev_offset = { o = Dot method_; _ } :: rev_offset' };
       _;
      } -> (
          match (base, rev_offset', tlval.offset) with
          | Var obj, [], _offset ->
              (* fun_exp = `obj.method(...)`, given lval = `this.x`
                 the instantiated l-value is `obj.x` *)
              Some (obj, tlval.offset, snd obj.ident)
          | VarSpecial (This, _), [], Ofld var :: offset ->
              (* fun_exp = `this.method(...)`, given lval = `this.x.y.z`
                 the instantiated l-value is `x.y.z`. *)
              Some (var, offset, snd method_.ident)
          | __else__ ->
              (* fun_exp = `this.obj.method(...)` (e.g.), given lval = `this.x.y`
                 the instantiated l-value is `obj.x.y`. *)
              let lval = IL.{ base; rev_offset = rev_offset' } in
              let* var, offset = Lval_env.normalize_lval lval in
              Some (var, offset @ tlval.offset, snd method_.ident))
      | __else__ ->
          log_error ();
          None)

(* HACK(implicit-taint-variables-in-env):
 * We have a function call with a taint variable, corresponding to a global or
 * a field in the same class as the caller, that reaches a sink. However, in
 * the caller we have no taint for the corresponding l-value.
 *
 * Why?
 * In 'find_instance_and_global_variables_in_fdef' we only add to the input-env
 * those globals and fields that occur in the  definition of a method, but just
 * because a global/field is not in there, it does not mean it's not in scope!
 *
 * What to do?
 * We can just propagate the very same taint variable, assuming that it is
 * implicitly in scope.
 *
 * Example (see SAF-1059):
 *
 *     string bad;
 *
 *     void test() {
 *         bad = "taint";
 *         // Thanks to this HACK we will know that calling 'foo'
 *         // here makes "taint" go into a sink.
 *         foo();
 *     }
 *
 *     void foo() {
 *         // We instantiate `bar` and we see 'bad ~~~> sink',
 *         // but `bad` is not in the environment, however we
 *         // know `bad` is a field in the same class as `foo`,
 *         // so we propagate it as-is.
 *         bar();
 *     }
 *
 *     // signature: bad ~~~> sink
 *     void bar() {
 *         sink(bad);
 *     }
 *
 * ALTERNATIVE:
 * In 'Deep_tainting.infer_taint_sigs_of_fdef', when we build
 * the taint input-env, we could collect all the globals and
 * class fields in scope, regardless of whether they occur or
 * not in the method definition. Main concern here is whether
 * input environments could end up being too big.
 *)
let fix_lval_taints_if_global_or_a_field_of_this_class (fun_exp : IL.exp)
    (lval : T.lval) lval_taints =
  let is_method_in_this_class =
    match fun_exp with
    | { e = Fetch { base = Var _method; rev_offset = [] }; _ } ->
        (* We're calling a `method` on the same instance of the caller,
           so `this.x` in the taint signature of the callee corresponds to
           `this.x` in the caller. *)
        true
    | __else__ -> false
  in
  match lval.base with
  | BArg _ -> lval_taints
  | BThis when not is_method_in_this_class -> lval_taints
  | BGlob _
  | BThis
    when not (Taints.is_empty lval_taints) ->
      lval_taints
  | BGlob _
  | BThis ->
      (* 'lval' is either a global variable or a field in the same class
       * as the caller of 'fun_exp', and no taints are found for 'lval':
       * we assume 'lval' is implicitly in the input-environment and
       * return it as a type variable. *)
      Taints.singleton { orig = Var lval; tokens = [] }

let combine_rest_args_taint (ts : (Taints.t * shape) list) : Taints.t * shape =
  let taints = List.fold_left Taints.union Taints.empty (List.map fst ts) in
  let shape =
    Obj (Fields.of_list
           (List.mapi
              (fun i (t, s) -> Taint.Oint i, Cell (Xtaint.of_taints t, s))
              ts))
  in
  (taints, shape) 

let instantiate_lval_using_shape lval_env fparams (fun_exp : IL.exp) args_taints
    lval : (Taints.t * shape) option =
  let { T.base; offset } = lval in
  let* base, offset =
    match base with
    | T.BArg pos -> Some (`Arg pos, offset)
    | BThis -> (
        (* TODO: Should we refactor this with 'instantiate_lval_using_actual_exps' ? *)
        match fun_exp with
        | {
         e = Fetch { base = Var obj; rev_offset = [ { o = Dot _method; _ } ] };
         _;
        } ->
            (* We're calling `obj.method`, so `this.x` is actually `obj.x` *)
            Some (`Var obj, offset)
        | { e = Fetch { base = Var var; rev_offset = [] }; _ } -> (
            (* We're calling a variable that holds a function (e.g., implicit block in Ruby).
             * For BThis with no offset, use the variable itself as it holds the receiver's taints.
             * For BThis with offset, this.x.y is just x.y *)
            match offset with
            | [] -> Some (`Var var, offset)
            | Ofld var :: offset -> Some (`Var var, offset)
            | (Oint _ | Ostr _ | Oany) :: _ -> None)
        | __else__ -> None)
    | BGlob var -> Some (`Var var, offset)
  in
  let* base_taints, base_shape =
    match base with
    | `Arg pos ->
        find_pos_in_actual_args
          ~err_ctx:(Display_IL.string_of_exp fun_exp)
          ~combine_rest_args:combine_rest_args_taint
          args_taints fparams pos
    | `Var var ->
        let* (Cell (xtaints, shape)) = Lval_env.find_var lval_env var in
        Some (Xtaint.to_taints xtaints, shape)
  in
  Shape.find_in_shape_poly ~taints:base_taints offset base_shape

(* What is the taint denoted by 'sig_lval' ? *)
let instantiate_lval lval_env fparams fun_exp args_exps
    (args_taints : (Taints.t * shape) IL.argument list) (sig_lval : T.lval) =
  match
    instantiate_lval_using_shape lval_env fparams fun_exp args_taints sig_lval
  with
  | Some (taints, shape) -> Some (taints, shape)
  | None -> (
      match args_exps with
      | None ->
          Log.warn (fun m ->
              m
                "Cannot find the taint&shape of %s because we lack the actual \
                 arguments"
                (T.show_lval sig_lval));
          None
      | Some args_exps ->
          (* We want to know what's the taint carried by 'arg_exp.x1. ... .xN'.
           * TODO: We should not need this when we cover everything with shapes,
           *   see 'lval_of_sig_lval'.
           *)
          let* var, offset, _obj =
            instantiate_lval_using_actual_exps fun_exp fparams args_exps
              sig_lval
          in
          let lval_taints, shape =
            match Lval_env.find_poly lval_env var offset with
            | None -> (Taints.empty, Bot)
            | Some (taints, shape) -> (taints, shape)
          in
          let lval_taints =
            lval_taints
            |> fix_lval_taints_if_global_or_a_field_of_this_class fun_exp
                 sig_lval
          in
          Some (lval_taints, shape))

(* This function is consuming the taint signature of a function to determine
   a few things:
   1) What is the status of taint in the current environment, after the function
      call occurs?
   2) Are there any effects that occur within the function due to taints being
      input into the function body, from the calling context?
*)
let rec instantiate_function_signature lval_env (taint_sig : Signature.t)
    ~callee ~(args : _ option)
    (args_taints : (Taints.t * shape) IL.argument list)
    ?(lookup_sig : (IL.exp -> int -> Signature.t option) option)
    ?(depth : int = 0) () : call_effects option =
  Log.debug (fun m ->
      m "INST_SIG: depth=%d, callee=%s, num_args_taints=%d, sig_params=%s"
        depth
        (Display_IL.string_of_exp callee)
        (List.length args_taints)
        (taint_sig.params |> List.map Signature.show_param |> String.concat ", "));
  let lval_to_taints lval =
    (* This function simply produces the corresponding taints to the
        given argument, within the body of the function.
    *)
    (* Our first pass will be to substitute the args for taints.
       We can't do this indiscriminately at the beginning, because
       we might need to use some of the information of the pre-substitution
       taints and the post-substitution taints, for instance the tokens.

       So we will isolate this as a specific step to be applied as necessary.
    *)
    let opt_taints_shape =
      instantiate_lval lval_env taint_sig.params callee args args_taints lval
    in
    Log.debug (fun m ->
        m ~tags:sigs_tag "- Instantiating %s: %s -> %s"
          (Display_IL.string_of_exp callee)
          (T.show_lval lval)
          (match opt_taints_shape with
          | None -> "nothing :/"
          | Some (taints, shape) ->
              spf "%s & %s" (T.show_taints taints) (show_shape shape)));
    opt_taints_shape
  in
  (* Instantiation helpers *)
  let taints_in_ctrl () = Lval_env.get_control_taints lval_env in
  let inst_var = { inst_lval = lval_to_taints; inst_ctrl = taints_in_ctrl } in
  let inst_taint_var taint = instantiate_taint_var inst_var taint in
  let subst_in_precondition = subst_in_precondition inst_var in
  let inst_trace =
    {
      add_call_to_trace_for_src = add_call_to_trace_if_callee_has_eorig ~callee;
      fix_token_trace_for_var = add_call_to_token_trace ~callee;
    }
  in
  let inst_taints taints =
    let result = instantiate_taints inst_var inst_trace taints in
    result
  in
  let inst_shape shape = instantiate_shape inst_var inst_trace shape in
  let inst_taints_and_shape (taints, shape) =
    let taints = inst_taints taints in
    let shape = inst_shape shape in
    (taints, shape)
  in
  (* Instatiate effects *)
  let inst_effect : Effect.t -> call_effect list = function
    | Effect.ToReturn { data_taints; data_shape; control_taints; return_tok } ->
        Log.debug (fun m ->
            m "INST_EFFECT: ToReturn BEFORE inst_taints: %d taints"
              (Taints.cardinal data_taints));
        let data_taints = inst_taints data_taints in
        let data_shape = inst_shape data_shape in
        let control_taints =
          (* No need to instantiate 'control_taints' because control taint variables
           * do not propagate through function calls... BUT instantiation also fixes
           * the call trace! *)
          inst_taints control_taints
        in
        Log.debug (fun m ->
            m "INST_EFFECT: ToReturn AFTER inst_taints: %d taints, control=%d, relevant=%b"
              (Taints.cardinal data_taints)
              (Taints.cardinal control_taints)
              (Shape.taints_and_shape_are_relevant data_taints data_shape));
        if
          Shape.taints_and_shape_are_relevant data_taints data_shape
          || not (Taints.is_empty control_taints)
        then
          [ ToReturn { data_taints; data_shape; control_taints; return_tok } ]
        else []
    | Effect.ToSink
        { taints_with_precondition = taints, requires; sink; merged_env } ->
        let taints =
          taints
          |> List.concat_map (fun { Effect.taint; sink_trace } ->
                 (* TODO: Use 'instantiate_taint' here too (note differences wrt the call trace). *)
                 match taint.T.orig with
                 | T.Src _ ->
                     (* Here, we do not modify the call trace or the taint.
                        This is because this means that, without our intervention, a
                        source of taint reaches the sink upon invocation of this function.
                        As such, we don't need to touch its call trace.
                     *)
                     (* Additionally, we keep this taint around, as compared to before,
                        when we assumed that only a single taint was necessary to produce
                        a finding.
                        Before, we assumed we could get rid of it because a
                        previous `effects_of_tainted_sink` call would have already
                        reported on this source. However, with interprocedural taint labels,
                        a finding may now be dependent on multiple such taints. If we were
                        to get rid of this source taint now, we might fail to report a
                        finding from a function call, because we failed to store the information
                        of this source taint within that function's taint signature.

                        e.g.

                        def bar(y):
                          foo(y)

                        def foo(x):
                          a = source_a
                          sink_of_a_and_b(a, x)

                        Here, we need to keep the source taint around, or our `bar` function
                        taint signature will fail to realize that the taint of `source_a` is
                        going into `sink_of_a_and_b`, and we will fail to produce a finding.
                     *)
                     let+ taint = taint |> subst_in_precondition in
                     [ { Effect.taint; sink_trace } ]
                 | Var _
                 | Shape_var _
                 | Control ->
                     let sink_trace =
                       add_call_to_trace_if_callee_has_eorig ~callee
                         taint.tokens sink_trace
                       ||| sink_trace
                     in
                     let+ call_taints, call_shape = inst_taint_var taint in
                     (* See NOTE(gather-all-taints) *)
                     let call_taints =
                       call_taints
                       |> Taints.union
                            (Shape.gather_all_taints_in_shape call_shape)
                     in
                     Taints.elements call_taints
                     |> List_.map (fun x -> { Effect.taint = x; sink_trace }))
        in
        if List_.null taints then []
        else
          [
            ToSink
              {
                taints_with_precondition = (taints, requires);
                sink;
                merged_env;
              };
          ]
    | Effect.ToLval (taints, dst_sig_lval) ->
        (* Taints 'taints' go into an argument of the call, by side-effect.
         * Right now this is mainly used to track taint going into specific
         * fields of the callee object, like `this.x = "tainted"`. *)
        let+ dst_var, dst_offset, tainted_tok =
          (* 'dst_lval' is the actual argument/l-value that corresponds
           * to the formal argument 'dst_sig_lval'. *)
          match args with
          | None ->
              Log.warn (fun m ->
                  m
                    "Cannot instantiate '%s' because we lack the actual \
                     arguments"
                    (T.show_lval dst_sig_lval));
              None
          | Some args ->
              instantiate_lval_using_actual_exps callee taint_sig.params args
                dst_sig_lval
        in
        let taints =
          let result =
            taints
            |> instantiate_taints
                 {
                   inst_lval = lval_to_taints;
                   (* Note that control taints do not propagate to l-values. *)
                   inst_ctrl = (fun _ -> Taints.empty);
                 }
                 {
                   add_call_to_trace_for_src =
                     add_call_to_trace_if_callee_has_eorig ~callee;
                   fix_token_trace_for_var =
                     add_lval_update_to_token_trace ~callee tainted_tok;
                 }
          in
          result
        in

        if Taints.is_empty taints then []
        else [ ToLval (taints, dst_var, dst_offset) ]
    | Effect.ToSinkInCall
        { callee = fun_exp; arg = fun_arg; args_taints = fun_args_taints } -> (
        Log.debug (fun m ->
            m ~tags:sigs_tag "- Instantiating %s: Call to function arg '%s'"
              (Display_IL.string_of_exp callee)
              (Display_IL.string_of_exp fun_exp));
        let fun_sig_opt =
          let fun_lval = T.lval_of_arg fun_arg in
          (* Get the actual function expression from args if available *)
          let actual_fun_exp =
            match args with
            | Some actual_args when fun_arg.index < List.length actual_args ->
                (match List.nth actual_args fun_arg.index with
                | IL.Unnamed exp -> Some exp
                | IL.Named (_, exp) -> Some exp)
            | _ -> None
          in
          Log.debug (fun m ->
              m "ToSinkInCall: actual_fun_exp = %s, lookup_sig = %s"
                (match actual_fun_exp with Some e -> Display_IL.string_of_exp e | None -> "None")
                (if Option.is_some lookup_sig then "Some" else "None"));
          (* Try to use the actual lambda expression to find its shape in lval_env *)
          let fun_sig_opt =
            match actual_fun_exp with
            | Some ({ IL.e = Fetch { base = Var var_name; rev_offset = []; _ }; _ }) ->
                (* Simple variable reference like _tmp:67 *)
                let taint_lval = { T.base = BGlob var_name; offset = [] } in
                Log.debug (fun m ->
                    m "ToSinkInCall: Trying lval_to_taints for var %s" (IL.str_of_name var_name));
                (match lval_to_taints taint_lval with
                | Some (_taints, Fun sig_) ->
                    Log.debug (fun m -> m "ToSinkInCall: Found signature in lval_env");
                    Some sig_
                | Some (_taints, _other_shape) ->
                    Log.debug (fun m -> m "ToSinkInCall: Found non-Fun shape in lval_env");
                    None
                | None ->
                    Log.debug (fun m -> m "ToSinkInCall: Not found in lval_env");
                    None)
            | _ ->
                Log.debug (fun m -> m "ToSinkInCall: actual_fun_exp is not a simple var reference");
                None
          in
          (* If we didn't find the lambda's shape, fall back to using the parameter lval *)
          let fun_sig_opt = match fun_sig_opt with
            | Some _ -> fun_sig_opt
            | None ->
                (match lval_to_taints fun_lval with
                | Some (_fun_taints, Fun fun_sig) ->
                    (* The '_fun_taints' are the taints (not its signature) of the actual
                     * function argument, and they are not used for instantiation, they are
                     * tracked by the caller like any other intra-procedural taint. *)
                    Some fun_sig
                | Some (_fun_taints, _non_Fun_shape) -> None
                | None -> None)
          in
          match fun_sig_opt with
          | Some fun_sig ->
              Some fun_sig
          | None ->
              (* No Fun shape found - try looking up signature from database if available *)
              (match lookup_sig, actual_fun_exp with
              | Some lookup_fn, Some actual_exp ->
                  (* Check if fun_exp is a method call (e.g., callback.apply) *)
                  let exp_to_lookup = match fun_exp.IL.e with
                    | Fetch { base = _; rev_offset = _ :: _ } ->
                        (* fun_exp is a method call like "callback.apply"
                         * We need to substitute the base with actual_exp to get "_tmp.apply" *)
                        (match actual_exp.IL.e with
                        | Fetch { base = actual_base; rev_offset = [] } ->
                            (* Construct a new expression with the actual base *)
                            (match fun_exp.IL.e with
                            | Fetch { base = _; rev_offset } ->
                                {
                                  IL.e = Fetch { base = actual_base; rev_offset };
                                  eorig = fun_exp.eorig;
                                }
                            | _ -> actual_exp)
                        | _ -> actual_exp)
                    | _ -> actual_exp
                  in
                  (* If exp_to_lookup is a temp var with NoOrig, try to extract callback from
                     callee's eorig which contains the original call expression *)
                  let exp_to_lookup =
                    match exp_to_lookup.IL.e, exp_to_lookup.eorig with
                    | Fetch { base = Var _; rev_offset = [] }, IL.NoOrig ->
                        (* Try to get callback from callee's original call expression *)
                        (match callee.eorig with
                        | IL.SameAs { G.e = G.Call (_, (_, orig_args, _)); _ } ->
                            (* Extract the argument at fun_arg.index from original AST args *)
                            (match List.nth_opt orig_args fun_arg.index with
                            | Some (G.Arg { G.e = G.N (G.Id (id, id_info)); _ }) ->
                                (* Simple callback: customForEach(arr, n, sink_callback) *)
                                let callback_name = AST_to_IL.var_of_id_info id id_info in
                                { IL.e = Fetch { base = Var callback_name; rev_offset = [] };
                                  eorig = IL.NoOrig }
                            | Some (G.Arg { G.e = G.Ref (_, { G.e = G.N (G.Id (id, id_info)); _ }); _ }) ->
                                (* Address-of callback: customForEach(arr, n, &sink_callback) *)
                                let callback_name = AST_to_IL.var_of_id_info id id_info in
                                { IL.e = Fetch { base = Var callback_name; rev_offset = [] };
                                  eorig = IL.NoOrig }
                            | _ -> exp_to_lookup)
                        | _ -> exp_to_lookup)
                    | _ -> exp_to_lookup
                  in
                  (* Try to look up the signature - assume arity matches the args_taints *)
                  let lookup_arity = List.length fun_args_taints in
                  Log.debug (fun m ->
                      m "TOSINKINCALL: Looking up signature for '%s' with arity %d"
                        (Display_IL.string_of_exp exp_to_lookup) lookup_arity);
                  (match lookup_fn exp_to_lookup lookup_arity with
                  | Some sig_ ->
                      Log.debug (fun m ->
                          m "TOSINKINCALL: Found signature for '%s'"
                            (Display_IL.string_of_exp exp_to_lookup));
                      Some sig_
                  | None ->
                      (* For anonymous classes, try looking up just the method name without the object *)
                      Log.debug (fun m ->
                          m "TOSINKINCALL: No signature found for '%s', trying method name only"
                            (Display_IL.string_of_exp exp_to_lookup));
                      (match exp_to_lookup.IL.e with
                      | Fetch { base = _; rev_offset = [{ o = Dot method_name; _ }] } ->
                          (* Try looking up just the method name *)
                          let method_only_exp = {
                            IL.e = Fetch { base = Var method_name; rev_offset = [] };
                            eorig = exp_to_lookup.eorig;
                          } in
                          Log.debug (fun m ->
                              m "TOSINKINCALL: Looking up method name only: '%s' with arity %d"
                                (Display_IL.string_of_exp method_only_exp) (List.length fun_args_taints));
                          (match lookup_fn method_only_exp (List.length fun_args_taints) with
                          | Some sig_ ->
                              Log.debug (fun m ->
                                  m "TOSINKINCALL: Found signature for method name '%s'"
                                    (Display_IL.string_of_exp method_only_exp));
                              Some sig_
                          | None ->
                              Log.err (fun m ->
                                  m "%s: Could not find the shape of function argument '%s', and no signature found"
                                    (Display_IL.string_of_exp callee)
                                    (Display_IL.string_of_exp exp_to_lookup));
                              None)
                      | _ ->
                          Log.err (fun m ->
                              m "%s: Could not find the shape of function argument '%s', and no signature found"
                                (Display_IL.string_of_exp callee)
                                (Display_IL.string_of_exp exp_to_lookup));
                          None))
              | _, _ ->
                  Log.err (fun m ->
                      m "%s: Could not find the shape of function argument '%s'"
                        (Display_IL.string_of_exp callee)
                        (T.show_arg fun_arg));
                  None)
        in
        (* Instantiate the args_taints *)
        let args_taints =
          fun_args_taints
          |> List_.map (function
               | IL.Unnamed (taints, shape) ->
                   IL.Unnamed (inst_taints_and_shape (taints, shape))
               | IL.Named (ident, (taints, shape)) ->
                   IL.Named (ident, inst_taints_and_shape (taints, shape)))
        in
        (* Handle the callback signature resolution *)
        match fun_sig_opt with
        | Some fun_sig ->
            Log.debug (fun m ->
                m ~tags:sigs_tag
                  "** %s: Instantiated function call '%s' arguments: %s -> %s"
                  (Display_IL.string_of_exp callee)
                  (Display_IL.string_of_exp fun_exp)
                  (Effect.show_args_taints fun_args_taints)
                  (Effect.show_args_taints args_taints));
            (* Check depth limit before recursing into callback *)
            if depth >= Limits_semgrep.taint_MAX_VISITS_PER_NODE then []
            else
            (* Pass through the outer args so we can extract the actual callback expression *)
            (match
               instantiate_function_signature lval_env fun_sig ~callee:fun_exp
                 ~args args_taints ?lookup_sig ~depth:(depth + 1) ()
             with
             | Some call_effects -> call_effects
             | None ->
                 (* Could not instantiate the callback signature, preserve ToSinkInCall *)
                 let callee_exp, updated_arg =
                   match args with
                   | Some actual_args when fun_arg.index < List.length actual_args ->
                       (match List.nth actual_args fun_arg.index with
                       | IL.Unnamed exp | IL.Named (_, exp) ->
                           (* Check if this expression is a parameter of the enclosing function *)
                           let arg_opt = match exp.IL.e with
                             | Fetch { base = Var var; rev_offset = [] } ->
                                 (* Check if this variable is in lval_env as a parameter *)
                                 let lval = { T.base = BGlob var; offset = [] } in
                                 (match lval_to_taints lval with
                                 | Some (taints, _shape) ->
                                     (* Look for a taint from a parameter *)
                                     let param_opt = taints |> Taints.elements |> List.find_map (fun t ->
                                       match t.T.orig with
                                       | Var { base = BArg arg; offset = [] } -> Some arg
                                       | _ -> None)
                                     in
                                     param_opt
                                 | None -> None)
                             | _ -> None
                           in
                           (exp, Option.value arg_opt ~default:fun_arg))
                   | _ -> (fun_exp, fun_arg)
                 in
                 Log.debug (fun m ->
                     m "%s: Could not instantiate signature of '%s', preserving ToSinkInCall effect with actual callee '%s' (arg index=%d)"
                       (Display_IL.string_of_exp callee)
                       (Display_IL.string_of_exp fun_exp)
                       (Display_IL.string_of_exp callee_exp)
                       updated_arg.index);
                 [ ToSinkInCall { callee = callee_exp; arg = updated_arg; args_taints } ])
        | None ->
            (* No signature found for callback (parameter during signature extraction).
             * Preserve the ToSinkInCall effect, but update arg to refer to the enclosing function's parameter. *)
            let callee_exp, updated_arg =
              match args with
              | Some actual_args when fun_arg.index < List.length actual_args ->
                  (match List.nth actual_args fun_arg.index with
                  | IL.Unnamed exp | IL.Named (_, exp) ->
                      (* Check if this expression is a parameter of the enclosing function *)
                      let arg_opt = match exp.IL.e with
                        | Fetch { base = Var var; rev_offset = [] } ->
                            (* Check if this variable is in lval_env as a parameter *)
                            let lval = { T.base = BGlob var; offset = [] } in
                            (match lval_to_taints lval with
                            | Some (taints, _shape) ->
                                (* Look for a taint from a parameter *)
                                let param_opt = taints |> Taints.elements |> List.find_map (fun t ->
                                  match t.T.orig with
                                  | Var { base = BArg arg; offset = [] } -> Some arg
                                  | _ -> None)
                                in
                                param_opt
                            | None -> None)
                        | _ -> None
                      in
                      (exp, Option.value arg_opt ~default:fun_arg))
              | _ -> (fun_exp, fun_arg)
            in
            Log.debug (fun m ->
                m "%s: No signature found for '%s', preserving ToSinkInCall effect with actual callee '%s' (arg index=%d)"
                  (Display_IL.string_of_exp callee)
                  (Display_IL.string_of_exp fun_exp)
                  (Display_IL.string_of_exp callee_exp)
                  updated_arg.index);
            [ ToSinkInCall { callee = callee_exp; arg = updated_arg; args_taints } ])
  in
  let effects_list = taint_sig.effects |> Effects.elements in
  let call_effects = effects_list |> List.concat_map inst_effect in
  Log.debug (fun m ->
      m ~tags:sigs_tag "Instantiated call to %s: %s"
        (Display_IL.string_of_exp callee)
        (show_call_effects call_effects));
  Some call_effects
