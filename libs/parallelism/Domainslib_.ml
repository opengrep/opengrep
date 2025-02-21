(* Parmap replacement module *)

module T = Domainslib.Task

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

(* This is a bit misleading. And I suspect it's not true, for example with
 * hyperthreading we might / could get e.g. 8 when the cores are 4? *)
let get_cpu_count () = Domain.recommended_domain_count ()

(* WARNING: Do not pass any [f] that does not expect to be uniquely
 * executing in a [Thread.t] until it produces a result. For example,
 * do not pass [f] that makes use of [Domainslib] functions that create
 * tasks. Our functions use Mutexes which do not work as expected in such
 * a context. Moreover, as explained below we make use of memprof-limits
 * and this will also not work in this context, for the same reasons for
 * which it will not work in [Lwt]. 
 * See https://github.com/ocaml-multicore/domainslib/issues/127. *)
let parmap _caps ?(chunksize=1) ~num_domains ~exception_handler f xs =
  (* NOTE: For now we require [chunk_size] to be 1, because each task may
   * make use of [Memprof_limits] functionality, which depends on thread-local
   * storage. If we bundle such [f] together, this will not work as expected
   * since more than one task can run on the same thread. *)
  assert (Int.equal chunksize 1);
  let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
  let xs_array = Array.of_list xs in
  let res_array = Array.make (Array.length xs_array) None in
  let f' x = wrap_result f ~exception_handler x in
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

