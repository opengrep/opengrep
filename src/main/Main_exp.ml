(* An entry point for the executable that:
 * - never falls back on python,
 * - always runs with --experimental,
 * - does not depend on argv[0]. *)

let first_hyphen_in_argv argv =
  Array.find_index
    (fun arg -> String.starts_with ~prefix:"-" arg)
    argv

let position_for_experimental_flag argv =
  match first_hyphen_in_argv argv with
  | None -> Array.length argv - 1
  | Some i -> i

let with_experimental_flag argv =
  let len = position_for_experimental_flag argv in
  Array.concat [
    Array.sub argv 0 len;
    [| "--experimental" |];
    Array.sub argv len (Array.length argv - len);
  ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let () =
  Cap.main (fun (caps : Cap.all_caps) ->
      let argv = CapSys.argv caps#argv in
      let exit_code =
        CLI.main
          (caps :> CLI.caps)
          (with_experimental_flag argv)
      in
      if not (Exit_code.Equal.ok exit_code) then
        Logs.info (fun m ->
            m "Error: %s\nExiting with error status %i: %s\n%!"
              exit_code.description exit_code.code
              (String.concat " " (Array.to_list argv)));
      CapStdlib.exit caps#exit exit_code.code)
