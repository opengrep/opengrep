(* return whether or not we should show a spinner *)
val should_show_spinner : unit -> bool

(*
  Show a spinner while waiting for the user to sign in.
  delay_ms is the total delay across all frames, in milliseconds.
  We show each frame for 1/100th of the total delay.
*)
val show_spinner : int -> unit
(* [takes_previous_line] says the caller has just printed a line for the
   spinner to animate and erase when it stops, so that it reads as a status
   of what is happening. Without it the spinner uses a line of its own. *)
val spinner_async : ?takes_previous_line:bool -> unit -> 'a Lwt.t
val erase_spinner : unit -> unit
