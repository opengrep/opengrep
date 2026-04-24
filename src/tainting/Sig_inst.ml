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
      arg_offset : Taint.offset list;
      args_taints : Effect.args_taints;
      guards : Effect_guard.Set.t;
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
  var_tokens @
  (match get_ident_of_callee callee with
    | None -> []
    | Some ident -> [ snd ident ]) @
  caller_tokens

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
  (* TODO: Use `get_ident_of_callee callee` to add the callee to the trace. *)
  var_tokens @ lval_tok :: caller_tokens

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
  Taints.bind taints (fun taint -> instantiate_taint inst_var inst_trace taint)

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
    | Arg (arg, off) -> (
        let lval = { T.base = T.BArg arg; offset = off } in
        match inst_var.inst_lval lval with
        | Some (_taints, shape) -> shape
        | None ->
            Log.warn (fun m ->
                m "Could not instantiate arg shape: %s" (T.show_lval lval));
            Arg (arg, off))
    | Fun _ as funTODO ->
        (* Right now a function shape can only come from a top-level function,
         * whose shape will not depend on the parameters of another enclosing
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

(* Walk a caller-side [IL.exp] by [fun_arg_offset] to recover the concrete
 * sub-expression at that path. Used when instantiating a [ToSinkInCall]
 * effect: the callee's signature says the callback lives at
 * [arg[i][fun_arg_offset]]; the surrounding signature-resolution code then
 * uses the resolved expression to find the callback's Fun signature (via
 * the signature DB's [lookup_sig] or, as a secondary fallback,
 * [lval_to_taints] if a Fun cell is stored in the current [lval_env]).
 *
 * Each [index_into] step handles:
 *   - [Composite (List|Tuple|Array|Set)] + [Oint] — positional access.
 *   - [RecordOrDict] + [Ofld|Ostr] — record/dict literal field access.
 *   - [Fetch var] + any offset — follows the variable's [id_svalue] when
 *     it is a [G.Sym] of a Container/Dict, walks the G.expr one level,
 *     and wraps the resolved [G.N] leaf back as an [IL.exp] so the fold
 *     continues uniformly. This covers aliased records like
 *     [let opts = {cb: handler, ...}; my_hof(opts)] where the structural
 *     argument at the call site is only a variable reference.
 *
 * Returns [(consumed, leftover)]:
 *   - [consumed] is the deepest expression resolved via the steps above.
 *   - [leftover] is the offset suffix we could not consume. In the common
 *     svalue-walk case this is empty. *)
let resolve_callee_expr (base_exp : IL.exp) (fun_arg_offset : Taint.offset list)
    : IL.exp * Taint.offset list =
  let field_name_matches (key : IL.exp) (target : string) =
    match key.IL.e with
    | IL.Literal (G.String (_, (s, _), _)) -> String.equal s target
    | _ -> false
  in
  let find_in_record entries target =
    List.find_map
      (function
        | IL.Field (fname, v)
          when String.equal (IL.str_of_name fname) target ->
            Some v
        | IL.Entry (k, v) when field_name_matches k target -> Some v
        | _ -> None)
      entries
  in
  let string_of_offset = function
    | Taint.Ofld name -> Some (IL.str_of_name name)
    | Taint.Ostr s -> Some s
    | _ -> None
  in
  let generic_key_matches (k : G.expr) (target : string) =
    match k.G.e with
    | G.L (G.String (_, (s, _), _)) -> String.equal s target
    | G.L (G.Atom (_, (s, _))) -> String.equal s target
    | G.N (G.Id ((s, _), _)) -> String.equal s target
    | _ -> false
  in
  let step_generic (g_exp : G.expr) (off : Taint.offset) : G.expr option =
    match (g_exp.G.e, off) with
    | ( G.Container
          ((G.List | G.Tuple | G.Array | G.Set), (_, xs, _)),
        Taint.Oint i )
      when i < List.length xs ->
        Some (List.nth xs i)
    | G.Container (G.Dict, (_, kvs, _)), off -> (
        match string_of_offset off with
        | None -> None
        | Some target ->
            List.find_map
              (fun kv ->
                match kv.G.e with
                | G.Container (G.Tuple, (_, [ k; v ], _))
                  when generic_key_matches k target ->
                    Some v
                | _ -> None)
              kvs)
    | G.Record (_, fields, _), off ->
        (* Record fields carry a Generic [Id] whose ident is the bare
         * field name (no sid resolution). Compare against the bare name
         * from the offset, not [IL.str_of_name] (which appends the sid). *)
        let target_opt =
          match off with
          | Taint.Ofld name -> Some (fst name.ident)
          | Taint.Ostr s -> Some s
          | _ -> None
        in
        (match target_opt with
        | None -> None
        | Some target ->
            List.find_map
              (fun f ->
                match f with
                | G.F
                    {
                      s =
                        G.DefStmt
                          ( { name = G.EN (G.Id ((s, _), _)); _ },
                            G.FieldDefColon { vinit = Some v; _ } );
                      _;
                    }
                  when String.equal s target ->
                    Some v
                | _ -> None)
              fields)
    | _ -> None
  in
  (* Convert a [G.expr] leaf (returned by [step_generic] after walking a
   * caller variable's [id_svalue]) back into an [IL.exp] so the outer
   * fold can continue uniformly.
   *
   * Handles:
   *   - [G.N (G.Id ...)]   -> [Fetch (Var ...)] — a name reference.
   *   - [G.L lit]          -> [Literal lit] — a literal value.
   *   - [G.Container ((List|Tuple|Array|Set), xs)]
   *                        -> [Composite (kind, xs)] where each [x]
   *                        is recursively converted; returns [None] if
   *                        any sub-expression does not convert.
   *   - [G.Container (Dict, kvs)]
   *                        -> [RecordOrDict entries] via [IL.Entry];
   *                        each entry's key and value are recursively
   *                        converted. *)
  let rec svalue_leaf_to_il_exp (leaf : G.expr) : IL.exp option =
    match leaf.G.e with
    | G.N (G.Id (ident, id_info)) ->
        let il_name = AST_to_IL.var_of_id_info ident id_info in
        Some
          {
            IL.e =
              IL.Fetch
                { IL.base = IL.Var il_name; rev_offset = [] };
            eorig = IL.SameAs leaf;
          }
    | G.L lit -> Some { IL.e = IL.Literal lit; eorig = IL.SameAs leaf }
    | G.Container
        (((G.List | G.Tuple | G.Array | G.Set) as gkind), (l, xs, r)) ->
        let kind =
          match gkind with
          | G.List -> IL.CList
          | G.Tuple -> IL.CTuple
          | G.Array -> IL.CArray
          | G.Set -> IL.CSet
          | G.Dict -> assert false
        in
        let rec convert_all = function
          | [] -> Some []
          | x :: rest -> (
              match svalue_leaf_to_il_exp x with
              | None -> None
              | Some y -> (
                  match convert_all rest with
                  | None -> None
                  | Some ys -> Some (y :: ys)))
        in
        Option.map
          (fun xs_il ->
            {
              IL.e = IL.Composite (kind, (l, xs_il, r));
              eorig = IL.SameAs leaf;
            })
          (convert_all xs)
    | G.Container (G.Dict, (_, kvs, _)) ->
        let rec convert_kvs = function
          | [] -> Some []
          | kv :: rest -> (
              match kv.G.e with
              | G.Container (G.Tuple, (_, [ k; v ], _)) -> (
                  match
                    (svalue_leaf_to_il_exp k, svalue_leaf_to_il_exp v)
                  with
                  | Some k_il, Some v_il ->
                      Option.map
                        (fun ys -> IL.Entry (k_il, v_il) :: ys)
                        (convert_kvs rest)
                  | _ -> None)
              | _ -> None)
        in
        Option.map
          (fun entries ->
            { IL.e = IL.RecordOrDict entries; eorig = IL.SameAs leaf })
          (convert_kvs kvs)
    | G.Record (_, fields, _) ->
        (* A record field [F(DefStmt({name=EN Id(s,idinfo); _},
         *   FieldDefColon{vinit=Some v; _}))] becomes
         * [IL.Field(il_name_of_s, v_il)]. Any other field kind
         * (no name, no [vinit], non-Id name) fails the conversion. *)
        let rec convert_fields = function
          | [] -> Some []
          | f :: rest -> (
              match f with
              | G.F
                  {
                    s =
                      G.DefStmt
                        ( {
                            name = G.EN (G.Id (ident, id_info));
                            _;
                          },
                          G.FieldDefColon { vinit = Some v; _ } );
                    _;
                  } -> (
                  match svalue_leaf_to_il_exp v with
                  | None -> None
                  | Some v_il ->
                      let il_name =
                        AST_to_IL.var_of_id_info ident id_info
                      in
                      Option.map
                        (fun ys -> IL.Field (il_name, v_il) :: ys)
                        (convert_fields rest))
              | _ -> None)
        in
        Option.map
          (fun entries ->
            { IL.e = IL.RecordOrDict entries; eorig = IL.SameAs leaf })
          (convert_fields fields)
    | _ -> None
  in
  let index_into (exp : IL.exp) (off : Taint.offset) : IL.exp option =
    match (exp.IL.e, off) with
    | ( IL.Composite
          ((IL.CList | IL.CTuple | IL.CArray | IL.CSet), (_, xs, _)),
        Taint.Oint i )
      when i < List.length xs ->
        Some (List.nth xs i)
    | IL.RecordOrDict entries, Taint.Ofld name ->
        find_in_record entries (IL.str_of_name name)
    | IL.RecordOrDict entries, Taint.Ostr s ->
        find_in_record entries s
    | IL.Fetch { base = Var x; rev_offset = []; _ }, off ->
        let result =
          match !(x.id_info.id_svalue) with
          | Some (G.Sym g_exp) ->
              Option.bind (step_generic g_exp off) svalue_leaf_to_il_exp
          | _ -> None
        in
        Log.debug (fun m ->
            m "RESOLVE_SVALUE: var=%s -> %s" (IL.str_of_name x)
              (match result with
              | Some exp -> Display_IL.string_of_exp exp
              | None -> "<none>"));
        result
    | _ -> None
  in
  List.fold_left
    (fun (acc, left) off ->
      match left with
      | [] -> (
          match index_into acc off with
          | Some sub -> (sub, [])
          | None -> (acc, [ off ]))
      | _ -> (acc, left @ [ off ]))
    (base_exp, []) fun_arg_offset

(* Convert an [IL.offset list] in reverse order (as stored on an
 * [IL.lval]) to a forward-order [Taint.offset list]. Returns [None] if
 * any step cannot be statically resolved — this should not happen for
 * cond [Fetch]es produced by [Dataflow_tainting]'s recogniser, which
 * already checks every step via [offset_is_resolvable], but the check
 * is repeated here defensively. *)
let t_offset_of_il_rev_offset (rev_offset : IL.offset list) :
    T.offset list option =
  let rec go acc = function
    | [] -> Some acc
    | o :: rest -> (
        match o.IL.o with
        | IL.Dot name -> go (T.Ofld name :: acc) rest
        | IL.Index { e = IL.Literal (G.Int pi); _ } -> (
            match Parsed_int.to_int_opt pi with
            | Some i -> go (T.Oint i :: acc) rest
            | None -> None)
        | IL.Index { e = IL.Literal (G.String (_, (s, _), _)); _ } ->
            go (T.Ostr s :: acc) rest
        | _ -> None)
  in
  go [] rev_offset

(* Substitute every free [Fetch] in [cond] whose base matches an entry
 * in [param_refs] (by [IL.equal_name]) with the caller-side expression
 * obtained by resolving the parameter's sig-position via [resolve_arg]
 * and walking the [Fetch]'s offset path with [resolve_callee_expr]. A
 * walk that stalls with leftover offsets rebuilds a [Fetch] whose base
 * is the consumed expression's own [Fetch] (if any), extended with the
 * leftover; otherwise the original [Fetch] is returned unchanged. *)
let substitute_free_fetches (param_refs : (IL.name * int) list)
    (resolve_arg : Taint.arg -> IL.exp option) (cond : IL.exp) : IL.exp =
  let ext (consumed : IL.exp) (leftover : T.offset list)
      (original : IL.exp) : IL.exp =
    match leftover with
    | [] -> consumed
    | _ -> (
        match T.rev_IL_offset_of_offset leftover with
        | None -> original
        | Some rev_extra -> (
            match consumed.IL.e with
            | IL.Fetch lval ->
                {
                  consumed with
                  IL.e =
                    IL.Fetch
                      {
                        lval with
                        rev_offset = rev_extra @ lval.rev_offset;
                      };
                }
            | _ -> original))
  in
  let rec walk (e : IL.exp) : IL.exp =
    match e.e with
    | IL.Fetch lval -> (
        match lval.base with
        | IL.Var name -> (
            match
              List.find_opt (fun (n, _) -> IL.equal_name n name) param_refs
            with
            | None -> e
            | Some (_, idx) -> (
                let taint_arg =
                  { Taint.name = fst name.ident; index = idx }
                in
                match resolve_arg taint_arg with
                | None -> e
                | Some actual_exp -> (
                    match t_offset_of_il_rev_offset lval.rev_offset with
                    | None -> e
                    | Some t_off ->
                        let consumed, leftover =
                          resolve_callee_expr actual_exp t_off
                        in
                        ext consumed leftover e)))
        | IL.VarSpecial _ | IL.Mem _ -> e)
    | IL.Operator (wop, args) ->
        { e with IL.e = IL.Operator (wop, List.map walk_arg args) }
    | IL.Literal _ | IL.Composite _ | IL.RecordOrDict _ | IL.Cast _
    | IL.FixmeExp _ ->
        e
  and walk_arg = function
    | IL.Unnamed sub -> IL.Unnamed (walk sub)
    | IL.Named (id, sub) -> IL.Named (id, walk sub)
  in
  walk cond

(* Classification of an effect's callee-frame guard set against the
 * caller's actuals and (when available) the outer function's formal
 * parameters. *)
type guard_decision =
  | Drop_effect
      (** Some guard reduced to [G.Lit (G.Bool false)]. *)
  | Keep_guards of Effect_guard.Set.t
      (** Effect kept. The returned set contains the survivors: each
          was either unknown at this call (and no rebinding was
          possible) or rebound into [outer_params]' frame. Guards that
          proved [G.Lit (G.Bool true)] are removed; guards that were
          unknown and not rebindable are dropped. *)

let classify_guards ~(lang : Lang.t)
    ?(outer_params : IL.param list option) resolve_arg
    (guards : Effect_guard.Set.t) : guard_decision =
  let eval_env =
    Eval_il_partial.mk_env lang Dataflow_var_env.VarMap.empty
  in
  Effect_guard.Set.fold
    (fun g acc ->
      match acc with
      | Drop_effect -> Drop_effect
      | Keep_guards kept -> (
          let substituted =
            substitute_free_fetches g.param_refs resolve_arg g.cond
          in
          let res = Eval_il_partial.eval eval_env substituted in
          Log.debug (fun m ->
              m "GUARD_EVAL: %s => %s" (Effect_guard.show g)
                (match res with
                | G.Lit (G.Bool (true, _)) -> "true"
                | G.Lit (G.Bool (false, _)) -> "false"
                | _ -> "unknown"));
          match res with
          | G.Lit (G.Bool (true, _)) -> Keep_guards kept
          | G.Lit (G.Bool (false, _)) -> Drop_effect
          | _ -> (
              match outer_params with
              | None -> Keep_guards kept
              | Some op -> (
                  match IL_helpers.cond_param_refs op substituted with
                  | None -> Keep_guards kept
                  | Some param_refs ->
                      let rebound =
                        { Effect_guard.cond = substituted; param_refs }
                      in
                      Log.debug (fun m ->
                          m "GUARD_REBIND: %s -> %s"
                            (Effect_guard.show g)
                            (Effect_guard.show rebound));
                      Keep_guards (Effect_guard.Set.add rebound kept)))))
    guards (Keep_guards Effect_guard.Set.empty)

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
  Log.debug (fun m ->
      m "INST_LVAL_SHAPE: base_taints=%d base_shape=%s offset=%s"
        (Taints.cardinal base_taints) (show_shape base_shape)
        (T.show_offset_list offset));
  Shape.find_in_shape_poly ~taints:base_taints offset base_shape

(* What is the taint denoted by 'sig_lval' ? *)
let instantiate_lval lval_env fparams fun_exp args_exps
    (args_taints : (Taints.t * shape) IL.argument list) (sig_lval : T.lval) =
  Log.debug (fun m ->
      m "INST_LVAL: resolving %s in args_taints=%d items, fparams=%s"
        (T.show_lval sig_lval) (List.length args_taints)
        (fparams |> List.map Signature.show_param |> String.concat ","));
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
let rec instantiate_function_signature ~(lang : Lang.t)
    ?(outer_params : IL.param list option) lval_env
    (taint_sig : Signature.t) ~callee ~(args : _ option)
    (args_taints : (Taints.t * shape) IL.argument list)
    ?(lookup_sig : (IL.exp -> int -> Signature.t option) option)
    ?(depth : int = 0) () : call_effects option =
  Log.debug (fun m ->
      m "INST_SIG: depth=%d, callee=%s, num_args_taints=%d, sig_params=%s"
        depth
        (Display_IL.string_of_exp callee)
        (List.length args_taints)
        (taint_sig.params |> List.map Signature.show_param |> String.concat ", "));
  args_taints
  |> List.iteri (fun i a ->
         let taints, shape =
           match a with
           | IL.Unnamed v | IL.Named (_, v) -> v
         in
         Log.debug (fun m ->
             m "INST_SIG:   arg[%d]: taints=%d shape=%s" i
               (Taints.cardinal taints) (show_shape shape)));
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
    instantiate_taints inst_var inst_trace taints
  in
  let inst_shape shape = instantiate_shape inst_var inst_trace shape in
  let inst_taints_and_shape (taints, shape) =
    let taints = inst_taints taints in
    let shape = inst_shape shape in
    (taints, shape)
  in
  (* Resolver mapping a callee-frame [Taint.arg] to the caller-side [IL.exp]
   * at that position. [None] when we have no concrete args for this call
   * (e.g. signature extraction of an enclosing function); in that case
   * guards stay unknown and effects are kept. *)
  let resolve_arg : Taint.arg -> IL.exp option =
    match args with
    | None -> fun _ -> None
    | Some args ->
        find_pos_in_actual_args args taint_sig.params
          ~combine_rest_args:combine_rest_args_exp
  in
  (* Instatiate effects *)
  let inst_effect : Effect.t -> call_effect list =
   fun eff ->
    match
      classify_guards ~lang ?outer_params resolve_arg (Effect.guards_of eff)
    with
    | Drop_effect -> []
    | Keep_guards out_guards ->
      match eff with
    | Effect.ToReturn { data_taints; data_shape; control_taints; return_tok; _ } ->
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
          [
            ToReturn
              {
                data_taints;
                data_shape;
                control_taints;
                return_tok;
                guards = out_guards;
              };
          ]
        else []
    | Effect.ToSink
        { taints_with_precondition = taints, requires; sink; merged_env; _ } ->
        Log.debug (fun m ->
            m "INST_SINK: entering %d input taints"
              (List.length taints));
        let taints =
          taints
          |> List.concat_map (fun { Effect.taint; sink_trace } ->
                 Log.debug (fun m ->
                     m "INST_SINK: processing taint %s" (T.show_taint taint));
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
                     Log.debug (fun m ->
                         m "INST_SINK: calling inst_taint_var on %s"
                           (T.show_taint taint));
                     let+ call_taints, call_shape = inst_taint_var taint in
                     Log.debug (fun m ->
                         m "INST_SINK: inst_taint_var returned %d taints, shape=%s"
                           (Taints.cardinal call_taints)
                           (show_shape call_shape));
                     (* See NOTE(gather-all-taints) *)
                     let call_taints =
                       call_taints
                       |> Taints.union
                            (Shape.gather_all_taints_in_shape call_shape)
                     in
                     Log.debug (fun m ->
                         m "INST_SINK: after shape gather: %d taints"
                           (Taints.cardinal call_taints));
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
                guards = out_guards;
              };
          ]
    | Effect.ToLval { taints; lval = dst_sig_lval; guards = _ } ->
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
          taints
          |> instantiate_taints
               { inst_lval = lval_to_taints;
                 (* Note that control taints do not propagate to l-values. *)
                 inst_ctrl = (fun _ -> Taints.empty); }
               { add_call_to_trace_for_src =
                   add_call_to_trace_if_callee_has_eorig ~callee;
                 fix_token_trace_for_var =
                   add_lval_update_to_token_trace ~callee tainted_tok; }
        in
        if Taints.is_empty taints then []
        else [ ToLval (taints, dst_var, dst_offset) ]
    | Effect.ToSinkInCall
        {
          callee = fun_exp;
          arg = fun_arg;
          arg_offset = fun_arg_offset;
          args_taints = fun_args_taints;
          guards = _;
        } -> (
        Log.debug (fun m ->
            m ~tags:sigs_tag "- Instantiating %s: Call to function arg '%s'"
              (Display_IL.string_of_exp callee)
              (Display_IL.string_of_exp fun_exp));
        let fun_lval =
          { T.base = T.BArg fun_arg; offset = fun_arg_offset }
        in
        (* If the callback lives at [fun_lval] but behind an outer parameter
         * (reached via an [Arg] shape in the enclosing frame), record that
         * rebind target so the preserve-ToSinkInCall path below can carry the
         * precise (arg, offset) forward instead of guessing via parameter-taint
         * heuristics. *)
        let rebind_arg_to_outer =
          match lval_to_taints fun_lval with
          | Some (_, Arg (outer_arg, outer_off)) ->
              Log.debug (fun m ->
                  m "REBIND: fun_lval=%s resolves to Arg(%s, %s)"
                    (T.show_lval fun_lval) (T.show_arg outer_arg)
                    (T.show_offset_list outer_off));
              Some (outer_arg, outer_off)
          | _ -> None
        in
        (* If [exp] is a variable reference whose taints carry a [BArg]
         * origin, return that outer parameter. Used by the preserve paths
         * below to rebind the preserved effect's [arg] field so the caller
         * sees it as referring to an enclosing-frame parameter. *)
        let enclosing_param_of_exp (exp : IL.exp) : Taint.arg option =
          match exp.IL.e with
          | Fetch { base = Var var; rev_offset = [] } -> (
              let lval = { T.base = BGlob var; offset = [] } in
              match lval_to_taints lval with
              | Some (taints, _shape) ->
                  taints
                  |> Taints.elements
                  |> List.find_map (fun t ->
                         match t.T.orig with
                         | Var { base = BArg arg; offset = [] } -> Some arg
                         | _ -> None)
              | None -> None)
          | _ -> None
        in
        let fun_sig_opt =
          (* Get the actual function expression from args if available. When
           * [fun_arg_offset] is non-empty (the callback was bound from
           * [arg[i]] in the callee), take the [i]-th element of the caller's
           * composite expression to recover the concrete callback. *)
          let actual_fun_exp, leftover_offset =
            match args with
            | Some actual_args when fun_arg.index < List.length actual_args ->
                let base_exp =
                  match List.nth actual_args fun_arg.index with
                  | IL.Unnamed exp -> exp
                  | IL.Named (_, exp) -> exp
                in
                let consumed, leftover =
                  resolve_callee_expr base_exp fun_arg_offset
                in
                Log.debug (fun m ->
                    m "RESOLVE_CALLEE: base=%s offset=%s -> consumed=%s leftover=%s"
                      (Display_IL.string_of_exp base_exp)
                      (T.show_offset_list fun_arg_offset)
                      (Display_IL.string_of_exp consumed)
                      (T.show_offset_list leftover));
                (Some consumed, leftover)
            | _ -> (None, fun_arg_offset)
          in
          Log.debug (fun m ->
              m "ToSinkInCall: actual_fun_exp = %s, lookup_sig = %s"
                (match actual_fun_exp with Some e -> Display_IL.string_of_exp e | None -> "None")
                (if Option.is_some lookup_sig then "Some" else "None"));
          (* Try to use the actual lambda expression to find its shape in lval_env *)
          let fun_sig_opt =
            match actual_fun_exp with
            | Some ({ IL.e = Fetch { base = Var var_name; rev_offset = []; _ }; _ }) ->
                (* Variable reference — look it up at [leftover_offset] so
                 * that callbacks reached via record/map field access
                 * (e.g. [opts[Ofld "cb"]]) resolve to the Fun cell stored
                 * in [lval_env] when the caller-side structural form
                 * could not be indexed through directly. *)
                let taint_lval =
                  { T.base = BGlob var_name; offset = leftover_offset }
                in
                Log.debug (fun m ->
                    m "ToSinkInCall: Trying lval_to_taints for var %s offset=%s"
                      (IL.str_of_name var_name)
                      (T.show_offset_list leftover_offset));
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
                Log.debug (fun m ->
                    m "TOSINKINCALL: Falling back to fun_lval lookup: %s"
                      (T.show_lval fun_lval));
                (match lval_to_taints fun_lval with
                | Some (_fun_taints, Fun fun_sig) ->
                    Log.debug (fun m ->
                        m "TOSINKINCALL: fun_lval resolved to Fun signature");
                    (* The '_fun_taints' are the taints (not its signature) of the actual
                     * function argument, and they are not used for instantiation, they are
                     * tracked by the caller like any other intra-procedural taint. *)
                    Some fun_sig
                | Some (_fun_taints, other_shape) ->
                    Log.debug (fun m ->
                        m "TOSINKINCALL: fun_lval resolved to non-Fun: %s"
                          (show_shape other_shape));
                    None
                | None ->
                    Log.debug (fun m -> m "TOSINKINCALL: fun_lval not found");
                    None)
          in
          match fun_sig_opt with
          | Some fun_sig ->
              Some fun_sig
          | None ->
              Log.debug (fun m ->
                  m
                    "TOSINKINCALL: fun_sig_opt=None, trying lookup_sig \
                     (lookup_sig=%s, actual_fun_exp=%s)"
                    (if Option.is_some lookup_sig then "Some" else "None")
                    (match actual_fun_exp with
                    | Some e -> Display_IL.string_of_exp e
                    | None -> "None"));
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
            else (
              Log.debug (fun m ->
                  m
                    "TOSINKINCALL: Recursively instantiating sig with \
                     args_taints=%s"
                    (Effect.show_args_taints args_taints));
              (* The callback invocation's actual args are not available here
               * (we only have the outer args of the enclosing call); pass None
               * so that any guards in the callback's signature stay unknown
               * rather than being evaluated against the wrong args. *)
              (match
                 instantiate_function_signature ~lang ?outer_params lval_env
                   fun_sig ~callee:fun_exp ~args:None args_taints ?lookup_sig
                   ~depth:(depth + 1) ()
               with
              | Some call_effects ->
                  Log.debug (fun m ->
                      m
                        "TOSINKINCALL: Recursive instantiation returned %d \
                         effects"
                        (List.length call_effects));
                  call_effects
             | None ->
                 (* Could not instantiate the callback signature, preserve ToSinkInCall *)
                 let callee_exp, updated_arg =
                   match args with
                   | Some actual_args
                     when fun_arg.index < List.length actual_args -> (
                       match List.nth actual_args fun_arg.index with
                       | IL.Unnamed exp | IL.Named (_, exp) ->
                           let arg_opt = enclosing_param_of_exp exp in
                           (exp, Option.value arg_opt ~default:fun_arg))
                   | _ -> (fun_exp, fun_arg)
                 in
                 Log.debug (fun m ->
                     m "%s: Could not instantiate signature of '%s', preserving ToSinkInCall effect with actual callee '%s' (arg index=%d)"
                       (Display_IL.string_of_exp callee)
                       (Display_IL.string_of_exp fun_exp)
                       (Display_IL.string_of_exp callee_exp)
                       updated_arg.index);
                 [
                   ToSinkInCall
                     {
                       callee = callee_exp;
                       arg = updated_arg;
                       arg_offset = fun_arg_offset;
                       args_taints;
                       guards = out_guards;
                     };
                 ])
              )
        | None ->
            (* No signature found for callback (parameter during signature extraction).
             * Preserve the ToSinkInCall effect, but update arg to refer to the enclosing function's parameter.
             * When [rebind_arg_to_outer] is set, we already learned from the
             * shape system that the callback is [outer_arg] + [outer_off] in
             * the enclosing frame, so rebind to that directly (precise). *)
            let callee_exp, updated_arg, updated_offset =
              match args with
              | Some actual_args
                when fun_arg.index < List.length actual_args -> (
                  match List.nth actual_args fun_arg.index with
                  | IL.Unnamed exp | IL.Named (_, exp) -> (
                      match rebind_arg_to_outer with
                      | Some (outer_arg, outer_off) ->
                          (exp, outer_arg, outer_off)
                      | None ->
                          let arg_opt = enclosing_param_of_exp exp in
                          ( exp,
                            Option.value arg_opt ~default:fun_arg,
                            fun_arg_offset )))
              | _ -> (fun_exp, fun_arg, fun_arg_offset)
            in
            Log.debug (fun m ->
                m "%s: No signature found for '%s', preserving ToSinkInCall effect with actual callee '%s' (arg=%s offset=%s)"
                  (Display_IL.string_of_exp callee)
                  (Display_IL.string_of_exp fun_exp)
                  (Display_IL.string_of_exp callee_exp)
                  (T.show_arg updated_arg)
                  (T.show_offset_list updated_offset));
            [
              ToSinkInCall
                {
                  callee = callee_exp;
                  arg = updated_arg;
                  arg_offset = updated_offset;
                  args_taints;
                  guards = out_guards;
                };
            ])
  in
  let effects_list = taint_sig.effects |> Effects.elements in
  let call_effects = effects_list |> List.concat_map inst_effect in
  (* Post-instantiation invariant: every [Effect_guard.t] on an output
   * [ToSink]/[ToReturn] must refer to [outer_params]. Guards that were
   * anchored in the callee's frame have been either evaluated to a
   * concrete bool (and consumed), rebound to [outer_params]' frame, or
   * dropped. When [outer_params] is absent, no guards can be rebound
   * and the output must have an empty [guards] set.
   * Log visibly rather than silently on violation. *)
  let guards_in_outer_frame (guards : Effect_guard.Set.t) : bool =
    match outer_params with
    | None -> Effect_guard.Set.is_empty guards
    | Some op ->
        Effect_guard.Set.for_all
          (fun g ->
            List.for_all
              (fun (n, _) -> Option.is_some (IL_helpers.param_index op n))
              g.param_refs)
          guards
  in
  call_effects
  |> List.iter (function
       | ToSink { guards; _ } when not (guards_in_outer_frame guards) ->
           Log.err (fun m ->
               m
                 "INVARIANT: post-instantiation ToSink carries guards %s \
                  not anchored in outer_params (callee=%s)"
                 (Effect_guard.show_set guards)
                 (Display_IL.string_of_exp callee))
       | ToReturn { guards; _ } when not (guards_in_outer_frame guards) ->
           Log.err (fun m ->
               m
                 "INVARIANT: post-instantiation ToReturn carries guards %s \
                  not anchored in outer_params (callee=%s)"
                 (Effect_guard.show_set guards)
                 (Display_IL.string_of_exp callee))
       | _ -> ());
  Log.debug (fun m ->
      m ~tags:sigs_tag "Instantiated call to %s: %s"
        (Display_IL.string_of_exp callee)
        (show_call_effects call_effects));
  Some call_effects
