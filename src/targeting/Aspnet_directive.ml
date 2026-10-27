(* See the mli for usage documentation *)

module Lexer = Aspnet_directive_lexer

let is_handler_file (file : Fpath.t) : bool =
  List.exists
    (fun (ext : string) -> Filename.check_suffix (Fpath.to_string file) ext)
    [ ".ashx"; ".asmx" ]

type header = { language : Lang.t option; code_start : int }

let rec next_token (lexbuf : Lexing.lexbuf) : Lexer.token =
  match Lexer.token lexbuf with
  | Whitespace -> next_token lexbuf
  | token -> token

(* The attributes of a directive as (name, value) pairs, from the first token
   after its '<%@'. An attribute without '=', such as the directive's own
   name 'WebHandler', has no value. *)
let rec attributes (lexbuf : Lexing.lexbuf) (token : Lexer.token)
    (acc : (string * string option) list) :
    (string * string option) list option =
  match token with
  | Directive_end -> Some (List.rev acc)
  | Attribute_name name -> (
      match next_token lexbuf with
      | Equal ->
          let value = Lexer.value lexbuf in
          attributes lexbuf (next_token lexbuf) ((name, Some value) :: acc)
      | next -> attributes lexbuf next ((name, None) :: acc))
  | Byte_order_mark
  | Whitespace
  | Directive_start
  | Equal
  | Other
  | Eof ->
      None

let rec directives (lexbuf : Lexing.lexbuf) (token : Lexer.token)
    (acc : (string * string option) list list) (code_start : int) :
    ((string * string option) list list * int) option =
  match token with
  | Directive_start -> (
      match attributes lexbuf (next_token lexbuf) [] with
      | Some directive ->
          let code_start = Lexing.lexeme_end lexbuf in
          directives lexbuf (next_token lexbuf) (directive :: acc) code_start
      | None -> None)
  | Byte_order_mark
  | Whitespace
  | Directive_end
  | Attribute_name _
  | Equal
  | Other
  | Eof -> (
      match acc with
      | [] -> None
      | _ :: _ -> Some (List.rev acc, code_start))

(* ASP.NET compiles a file whose directives carry no Language attribute in
   the default language of web.config, which is VB unless web.config sets
   another. web.config is not read. *)
let language_of_directives (directives : (string * string option) list list) :
    Lang.t option =
  let language_attribute =
    List.concat directives
    |> List.find_map (fun ((name, value) : string * string option) ->
           if String.equal (String.lowercase_ascii name) "language" then value
           else None)
  in
  match language_attribute with
  | None -> Some Lang.Vb
  | Some language -> (
      match String.lowercase_ascii language with
      | "c#"
      | "cs"
      | "csharp" ->
          Some Lang.Csharp
      | "vb"
      | "vbs"
      | "visualbasic"
      | "vbscript" ->
          Some Lang.Vb
      | _ -> None)

let scan (text : string) : header option =
  let lexbuf = Lexing.from_string text in
  let first_token =
    match Lexer.token lexbuf with
    | Byte_order_mark
    | Whitespace ->
        next_token lexbuf
    | token -> token
  in
  directives lexbuf first_token [] 0
  |> Option.map
       (fun ((directives, code_start) : (string * string option) list list * int) ->
         { language = language_of_directives directives; code_start })

let blank_header (text : string) : string =
  match scan text with
  | None -> text
  | Some { code_start; _ } ->
      String.mapi
        (fun (i : int) (c : char) ->
          if i < code_start && not (Char.equal c '\n' || Char.equal c '\r') then
            ' '
          else c)
        text
