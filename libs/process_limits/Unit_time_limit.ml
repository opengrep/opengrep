(*
   Unit tests for Time_limit
*)

(* Allocates until [seconds] of wall-clock time have passed, so that a time
   limit can interrupt it. *)
let busy_for (seconds : float) () : unit =
  let deadline = Unix.gettimeofday () +. seconds in
  let rec loop () =
    if Unix.gettimeofday () < deadline then (
      ignore (Sys.opaque_identity (List.init 100 Fun.id));
      loop ())
  in
  loop ()

let test_tasks_in_sequence (caps : < Cap.time_limit >) () =
  let shared = Time_limit.create_shared_limit 1.0 in
  let finished =
    List.init 3 (fun (_ : int) ->
        Option.is_some
          (Time_limit.with_shared_limit caps ~name:"test" shared
             (busy_for 0.4)))
  in
  Alcotest.(check (list bool)) "the third task of 0.4 s is stopped"
    [ true; true; false ] finished

let test_tasks_overlap (caps : < Cap.time_limit >) () =
  let shared = Time_limit.create_shared_limit 1.0 in
  let finished =
    List.init 2 (fun (_ : int) ->
        Domain.spawn (fun () ->
            Option.is_some
              (Time_limit.with_shared_limit caps ~name:"test" shared
                 (busy_for 0.6))))
    |> List.map Domain.join
  in
  Alcotest.(check (list bool))
    "two tasks of 0.6 s at the same time both finish" [ true; true ] finished

let test_no_time_left (caps : < Cap.time_limit >) () =
  let shared = Time_limit.create_shared_limit 0.5 in
  Alcotest.(check bool) "a task of 1 s is stopped" false
    (Option.is_some
       (Time_limit.with_shared_limit caps ~name:"test" shared (busy_for 1.0)));
  let ran = Atomic.make false in
  Alcotest.(check bool) "a task started with no time left returns None" false
    (Option.is_some
       (Time_limit.with_shared_limit caps ~name:"test" shared (fun () ->
            Atomic.set ran true)));
  Alcotest.(check bool) "a task started with no time left does not run" false
    (Atomic.get ran)

let tests (caps : < Cap.time_limit >) : Testo.t list =
  Testo.categorize "Time_limit"
    [
      Testo.create "tasks of one shared limit run in sequence"
        (test_tasks_in_sequence caps);
      Testo.create "tasks of one shared limit overlap"
        (test_tasks_overlap caps);
      Testo.create "a task started after the shared limit is used up"
        (test_no_time_left caps);
    ]
