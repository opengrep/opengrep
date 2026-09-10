(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The wordmark, for the skins that open with one.
 *
 * The mark is drawn with box characters, three rows of the same width so
 * that the name, the tagline and the version line up in a column beside it.
 *)

(* each row padded to the width of the widest, so the text column is square *)
let mark = [ " ╭┮┭╮"; "┌┼┼┼┘"; "╰┶┵╯ " ]
let tagline = "the open source static code analysis engine"

(* Only the name is picked out; the mark and the lines beside it are plain,
   so the banner does not compete with the report under it. *)
let pp ~(margin : string) ppf : unit =
  let row (glyphs : string) (pp_text : Format.formatter -> unit) : unit =
    Fmt.pf ppf "%s%s %t@." margin glyphs pp_text
  in
  let plain (text : string) ppf : unit = Fmt.pf ppf "%s" text in
  match mark with
  | [ top; middle; bottom ] ->
      row top (fun ppf ->
          Fmt.pf ppf "%a" Fmt.(styled `Bold string) "Opengrep");
      row middle (plain tagline);
      row bottom (plain (Printf.sprintf "version %s" Version.version))
  | _else_ -> ()
