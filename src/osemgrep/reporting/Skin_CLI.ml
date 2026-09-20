open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The --skin flag, shared by the subcommands that print a text report. *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let o_skin : Skin.name Cmdliner.Term.t =
  (* Each skin describes itself, so that the help cannot say one thing
     while the report does another. *)
  let skins =
    Skin.all_names
    |> List_.map (fun ((label : string), (name : Skin.name)) ->
           let (module S : Skin.S) = Skins.resolve name in
           spf "$(b,%s): %s" label S.doc)
    |> String.concat " "
  in
  Cmdliner_.enum_with_env [ "skin" ] ~env:"OPENGREP_SKIN" ~default:Skin.default
    ~names:Skin.all_names
    ~doc:
      (spf
         {|Which look the text report has. %s Also settable with $(b,OPENGREP_SKIN).|}
         skins)
