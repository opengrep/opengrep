(* See Lazy_with_restart.mli for the rationale (a lazy that restarts its
   computation instead of memoizing an exception). *)

type 'a state =
  | Not_forced of (unit -> 'a)
  | Forced of 'a

type 'a t = { mutable state : 'a state }

let from_fun f = { state = Not_forced f }

let from_val v = { state = Forced v }

let force t =
  match t.state with
  | Forced v -> v
  | Not_forced f ->
      (* If [f ()] raises, we never reach the assignment below, so [t.state]
         stays [Not_forced] and the next [force] re-runs [f]. *)
      let v = f () in
      t.state <- Forced v;
      v

let is_val t =
  match t.state with
  | Forced _ -> true
  | Not_forced _ -> false
