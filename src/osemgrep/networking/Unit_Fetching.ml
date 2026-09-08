(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let t = Testo.create

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let real_fetch_tests caps =
  let fetch_ocaml_rules () =
    match
      Rule_fetching.rules_from_dashdash_config ~rewrite_rule_ids:false
        caps (Rules_config.R (Pack "ocaml"))
    with
    | [ { rules; _ } ], [] ->
        Alcotest.(check bool) "fetch ocaml rules" true (not @@ List_.null rules)
    | _ -> Alcotest.fail "fetch ocaml rules; got no rules or got rule errors"
  in
  Testo.categorize "fetch tests"
    [
      t "fetch ocaml rules 1" fetch_ocaml_rules;
      t "fetch ocaml rules 2" fetch_ocaml_rules;
      t "fetch ocaml rules 3" fetch_ocaml_rules;
      t "fetch ocaml rules 4" fetch_ocaml_rules;
      t "fetch ocaml rules 5" fetch_ocaml_rules;
    ]

(* The registry endpoint that each shortcut accepted by --config asks for.
   No request is made; only the URL is built. *)
let registry_url_tests () =
  let base : string = "https://registry.invalid" in
  let check (name : string) (kind : Rules_config.registry_config_kind)
      (path : string) : unit =
    let url : string =
      Semgrep_envvars.with_envvar "SEMGREP_URL" base (fun () ->
          Uri.to_string (Semgrep_Registry.url_of_registry_config_kind kind))
    in
    Alcotest.(check string) name (base ^ path) url
  in
  check "auto" Rules_config.Auto "/c/p/default";
  check "r2c" Rules_config.R2c "/c/p/r2c";
  check "a pack" (Rules_config.Pack "default") "/c/p/default";
  check "a ruleset" (Rules_config.Registry "python") "/c/r/python";
  check "a snippet" (Rules_config.Snippet "aBcDeF") "/c/s/aBcDeF"

let registry_tests =
  Testo.categorize "registry tests"
    [ t "the url of each registry shortcut" registry_url_tests ]

let tests caps =
  Testo.categorize_suites "OSemgrep Fetch"
    [ real_fetch_tests caps; registry_tests ]
