open Ast_go

type token =
  | Lparen
  | Rparen
  | Not
  | And
  | Or
  | Tag of string

let starts_with_field ~(prefix : string) (text : string) : bool =
  let length = String.length prefix in
  String.starts_with ~prefix text
  && (Int.equal (String.length text) length
     ||
     match text.[length] with
     | ' '
     | '\t' ->
         true
     | _ -> false)

let strip_prefix (prefix : string) (text : string) : string =
  let length = String.length prefix in
  String.sub text length (String.length text - length)

let go_build_prefix : string = "//go:build"

let is_go_build_comment (text : string) : bool =
  starts_with_field ~prefix:go_build_prefix (String.trim text)

let plus_build_fields (text : string) : string list option =
  let text = String.trim text in
  if String.starts_with ~prefix:"//" text then
    match
      String.split_on_char ' '
        (String.map
           (fun (c : char) -> if Char.equal c '\t' then ' ' else c)
           (strip_prefix "//" text))
      |> List.filter (fun (field : string) -> not (String.equal field ""))
    with
    | "+build" :: options -> Some options
    | _ -> None
  else None

let is_tag_char (c : char) : bool =
  match c with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '.' ->
      true
  | _ -> false

let is_tag (text : string) : bool =
  (not (String.equal text "")) && String.for_all is_tag_char text

let tokenise (text : string) : token list option =
  let length = String.length text in
  let rec tag_end (i : int) : int =
    if i < length && is_tag_char text.[i] then tag_end (i + 1) else i
  in
  let doubled (i : int) (c : char) : bool =
    i + 1 < length && Char.equal text.[i + 1] c
  in
  let rec scan (i : int) (tokens : token list) : token list option =
    if i >= length then Some (List.rev tokens)
    else
      match text.[i] with
      | ' '
      | '\t'
      | '\r'
      | '\n' ->
          scan (i + 1) tokens
      | '(' -> scan (i + 1) (Lparen :: tokens)
      | ')' -> scan (i + 1) (Rparen :: tokens)
      | '!' -> scan (i + 1) (Not :: tokens)
      | '&' when doubled i '&' -> scan (i + 2) (And :: tokens)
      | '|' when doubled i '|' -> scan (i + 2) (Or :: tokens)
      | c when is_tag_char c ->
          let stop = tag_end i in
          scan stop (Tag (String.sub text i (stop - i)) :: tokens)
      | _ -> None
  in
  scan 0 []

let parse_go_build ((text : string), (tok : tok)) : build_constraint option =
  let rec disjunction (tokens : token list) :
      (build_constraint * token list) option =
    Option.bind (conjunction tokens)
      (fun ((left : build_constraint), (rest : token list)) ->
        match rest with
        | Or :: more ->
            Option.map
              (fun ((right : build_constraint), (rest : token list)) ->
                (BuildOr (left, right), rest))
              (disjunction more)
        | _ -> Some (left, rest))
  and conjunction (tokens : token list) : (build_constraint * token list) option
      =
    Option.bind (unary tokens)
      (fun ((left : build_constraint), (rest : token list)) ->
        match rest with
        | And :: more ->
            Option.map
              (fun ((right : build_constraint), (rest : token list)) ->
                (BuildAnd (left, right), rest))
              (conjunction more)
        | _ -> Some (left, rest))
  and unary (tokens : token list) : (build_constraint * token list) option =
    match tokens with
    | Not :: rest ->
        Option.map
          (fun ((inner : build_constraint), (rest : token list)) ->
            (BuildNot inner, rest))
          (unary rest)
    | Lparen :: rest ->
        Option.bind (disjunction rest)
          (fun ((inner : build_constraint), (rest : token list)) ->
            match rest with
            | Rparen :: rest -> Some (inner, rest)
            | _ -> None)
    | Tag tag :: rest -> Some (BuildTag (tag, tok), rest)
    | _ -> None
  in
  Option.bind
    (tokenise (strip_prefix go_build_prefix (String.trim text)))
    (fun (tokens : token list) ->
      match disjunction tokens with
      | Some (parsed, []) -> Some parsed
      | Some (_, _ :: _)
      | None ->
          None)

let reduce
    (combine : build_constraint -> build_constraint -> build_constraint)
    (parts : build_constraint option list) : build_constraint option =
  match parts with
  | [] -> None
  | first :: rest ->
      List.fold_left
        (fun (combined : build_constraint option)
             (part : build_constraint option) ->
          Option.bind combined (fun (combined : build_constraint) ->
              Option.map (combine combined) part))
        first rest

let parse_plus_build ((text : string), (tok : tok)) : build_constraint option =
  let term (written : string) : build_constraint option =
    if String.starts_with ~prefix:"!" written then
      let tag = strip_prefix "!" written in
      if is_tag tag then Some (BuildNot (BuildTag (tag, tok))) else None
    else if is_tag written then Some (BuildTag (written, tok))
    else None
  in
  Option.bind (plus_build_fields text) (fun (options : string list) ->
      reduce
        (fun (left : build_constraint) (right : build_constraint) ->
          BuildOr (left, right))
        (List.map
           (fun (option : string) ->
             reduce
               (fun (left : build_constraint) (right : build_constraint) ->
                 BuildAnd (left, right))
               (List.map term (String.split_on_char ',' option)))
           options))

let with_header_constraints (comments : string wrap list) (program : program) :
    program =
  match
    List.find_map
      (fun (decl : top_decl) ->
        match decl with
        | Package (tpackage, _) -> Some tpackage
        | _ -> None)
      program
  with
  | None -> program
  | Some tpackage ->
      let package_start = Tok.bytepos_of_tok tpackage in
      let package_line = Tok.line_of_tok tpackage in
      let header =
        List.filter
          (fun ((_ : string), (tok : tok)) ->
            Tok.bytepos_of_tok tok < package_start)
          comments
      in
      let line_span (((text : string), (tok : tok)) : string wrap) : int * int =
        let first = Tok.line_of_tok tok in
        ( first,
          String.fold_left
            (fun (last : int) (c : char) ->
              if Char.equal c '\n' then last + 1 else last)
            first (String.trim text) )
      in
      let spans = List.map line_span header in
      let rec blank_line_before_package (line : int) : bool =
        line < package_line
        && ((not
               (List.exists
                  (fun ((first : int), (last : int)) ->
                    first <= line && line <= last)
                  spans))
           || blank_line_before_package (line + 1))
      in
      let parsed =
        match
          List.filter
            (fun ((text : string), (_ : tok)) -> is_go_build_comment text)
            header
        with
        | [] ->
            List.filter_map
              (fun (comment : string wrap) ->
                if blank_line_before_package (snd (line_span comment) + 1) then
                  Option.map
                    (fun (parsed : build_constraint) -> (snd comment, parsed))
                    (parse_plus_build comment)
                else None)
              header
        | go_build ->
            List.filter_map
              (fun (comment : string wrap) ->
                Option.map
                  (fun (parsed : build_constraint) -> (snd comment, parsed))
                  (parse_go_build comment))
              go_build
      in
      List.map
        (fun ((tok : tok), (parsed : build_constraint)) ->
          BuildConstraint (tok, parsed))
        parsed
      @ program
