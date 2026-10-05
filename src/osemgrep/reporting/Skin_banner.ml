(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The wordmark of the skins that start with a banner: three rows of box
 * characters of equal width, beside which the name, the tagline and the
 * version form a column.
 *)

(* every row has the same width, so that the text column is aligned *)
let mark = [ " ╭┮┭╮"; "┌┼┼┼┘"; "╰┶┵╯ " ]
let tagline = "the open source static code analysis engine"

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
