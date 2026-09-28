(* Parmap replacement module *)

module T = Domainslib.Task

(* A minor collection stops every domain, so with many domains allocating
   the default minor heap (256k words) keeps them in that barrier most of
   the time. Each domain sets its own: the runtime gives a spawned domain
   the startup default, never the parent's setting. 4M words removes most
   of the barrier; larger buys nothing. *)
let minor_heap_words = 4_000_000

(* Once per domain: the key's initialiser runs on the first [get] in each
   domain. *)
let minor_heap_set : unit Domain.DLS.key =
  Domain.DLS.new_key (fun () ->
      Gc.set { (Gc.get ()) with Gc.minor_heap_size = minor_heap_words })

(* From the [Parmap_] module. *)
let wrap_result f ~exception_handler x =
  try Ok (f x) with
  | exn ->
      let e = Exception.catch exn in
      (* From marshal.mli in the OCaml stdlib:
       *  "Values of extensible variant types, for example exceptions (of
       *  extensible type [exn]), returned by the unmarshaller should not be
       *  pattern-matched over through [match ... with] or [try ... with],
       *  because unmarshalling does not preserve the information required for
       *  matching their constructors. Structural equalities with other
       *  extensible variant values does not work either.  Most other uses such
       *  as Printexc.to_string, will still work as expected."
       *)
      (* Because of this we cannot just catch the exception here and return
         it, as then it won't be super usable. Instead we ask the user of the
         library to handle it in the process, since then they can pattern
         match on it. They can choose to convert it to a string, a different
         datatype etc. *)
      Error (exception_handler x e)

(* This is a bit misleading. In fact ocaml's [runtime/domain.c] defines this
 * to return the number of CPUs the process is allowed to run on, which may
 * be less than the real number of CPUs. *)
let get_cpu_count () = Domain.recommended_domain_count ()

(* The factor of Domainslib's [parallel_for] default chunk size,
   n_tasks / (8 * n_domains). *)
let batches_per_domain = 8

let batches ~(weight : 'a -> int) ~(num_domains : int) (items : 'a list)
    : 'a list list =
  let total =
    List.fold_left (fun (sum : int) item -> sum + weight item) 0 items
  in
  (* The job count reaches the dispatch as given; zero or a negative value
     runs sequentially, on one domain. *)
  let divisor = batches_per_domain * Int.max 1 num_domains in
  let target = (total + divisor - 1) / divisor in
  let closed, current, _ =
    List.fold_left
      (fun (closed, current, (current_weight : int)) item ->
        let item_weight = weight item in
        match current with
        | _ :: _ when current_weight + item_weight > target ->
            (List.rev current :: closed, [ item ], item_weight)
        | _ -> (closed, item :: current, current_weight + item_weight))
      ([], [], 0) items
  in
  List.rev
    (match current with
    | [] -> closed
    | _ :: _ -> List.rev current :: closed)

(* WARNING: Do not pass any [f] that does not expect to be uniquely
 * executing in a [Thread.t] until it produces a result. For example,
 * do not pass [f] that makes use of [Domainslib] functions that create
 * nested tasks. Our functions use Mutexes which do not work as expected in
 * such a context. Moreover, as explained below we make use of memprof-limits
 * and this will also not work in this context, for the same reasons for
 * which it will not work in [Lwt]. 
 * See https://github.com/ocaml-multicore/domainslib/issues/127. *)
let parmap _caps ?(chunksize=1) ~num_domains ~exception_handler f xs =
  (* NOTE: For now we require [chunk_size] to be 1, because each task may
   * make use of [Memprof_limits] functionality, which depends on thread-local
   * storage. If we bundle such [f] together, this will not work as expected
   * since more than one task can run on the same thread. *)
  assert (Int.equal chunksize 1);
  (* It can be detrimental to performance if we go above the CPU count, so we
   * place an upper bound. TODO: Add a log when this happens? *)
  let num_domains = min num_domains (get_cpu_count ()) in
  (* On this domain first: it raises the reservation while it is the only
     domain, and it takes tasks too. *)
  Domain.DLS.get minor_heap_set;
  let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
  let xs_array = Array.of_list xs in
  let res_array = Array.make (Array.length xs_array) None in
  let f' x =
    Domain.DLS.get minor_heap_set;
    wrap_result f ~exception_handler x
  in
  Common.protect ~finally:(fun () -> T.teardown_pool pool) (fun () ->
      T.run pool (fun () ->
          T.parallel_for pool ~start:0 ~finish:(Array.length xs_array - 1)
            (* TODO: Maybe clean up TLS state after each task?
             * We can use [Globals.reset ()] but we may need to expand
             * it to cover all state that must be reset. For memprof-limits
             * this is more complex, which could explain why we get races
             * even with [chunk_size] equal to 1. Because it may be the same
             * thread that runs > 1 task, in sequence since [chunk_size = 1],
             * as they are fed into each domain. *)
            ~chunk_size:chunksize
            ~body:(fun i -> res_array.(i) <- Some (f' xs_array.(i)))));
  Array.map Option.get res_array |> Array.to_list

let parmap_batches (caps : < Cap.fork >) ~(ncores : int) ~exception_handler
    fn batches =
  let n = List.length batches in
  if ncores <= 1 || n <= 1 then
    List_.map (wrap_result fn ~exception_handler) batches
  else
    parmap caps ~num_domains:(min ncores n) ~exception_handler fn batches

