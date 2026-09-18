(* Wall-clock accumulators for parts of the taint dataflow, summed across
   every call and every domain.  [Interfile_dispatch] logs [report] with the
   interfile phase timings. *)
let secs : (string, float) Hashtbl.t = Hashtbl.create 8
let mutex = Mutex.create ()

let accum (name : string) (f : unit -> 'a) : 'a =
  let res, t = Common.with_time f in
  Mutex.lock mutex;
  let prev = Option.value (Hashtbl.find_opt secs name) ~default:0. in
  Hashtbl.replace secs name (prev +. t);
  Mutex.unlock mutex;
  res

let report () : string =
  Mutex.lock mutex;
  let xs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) secs [] in
  Mutex.unlock mutex;
  xs
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> List.map (fun (k, v) -> Printf.sprintf "%s %.1fs" k v)
  |> String.concat ", "
