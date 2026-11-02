let is_octal_digit (c : char) : bool =
  match c with
  | '0' .. '7' -> true
  | _ -> false

let hex_value (s : string) (start : int) (count : int) : int option =
  if start + count <= String.length s then
    let digits = String.sub s start count in
    if String.for_all Char.Ascii.is_hex_digit digits then
      Some
        (String.fold_left
           (fun (value : int) (c : char) ->
             (value * 16) + Char.Ascii.hex_digit_to_int c)
           0 digits)
    else None
  else None

(* The number of octal digits of [s] at [start], at most three. *)
let octal_length (s : string) (start : int) : int =
  let rec count (n : int) : int =
    if n < 3 && start + n < String.length s && is_octal_digit s.[start + n] then
      count (n + 1)
    else n
  in
  count 0

(* \N{...}, an escape whose value valid UTF-8 cannot hold (a surrogate, or a
   byte from 0x80 in a bytes literal), and a malformed escape, which CPython
   rejects, are kept as text. *)
let unescaped ~(bytes : bool) (s : string) : string =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec scan (i : int) : unit =
    if i < len then
      if Char.equal s.[i] '\\' && i + 1 < len then escape i
      else (
        Buffer.add_char buf s.[i];
        scan (i + 1))
  and escape (i : int) : unit =
    let simple (c : char) : unit =
      Buffer.add_char buf c;
      scan (i + 2)
    in
    match s.[i + 1] with
    | '\n' -> scan (i + 2)
    | '\r' ->
        scan (if i + 2 < len && Char.equal s.[i + 2] '\n' then i + 3 else i + 2)
    | '\\' -> simple '\\'
    | '\'' -> simple '\''
    | '"' -> simple '"'
    | 'a' -> simple '\x07'
    | 'b' -> simple '\b'
    | 'f' -> simple '\x0c'
    | 'n' -> simple '\n'
    | 'r' -> simple '\r'
    | 't' -> simple '\t'
    | 'v' -> simple '\x0b'
    | '0' .. '7' ->
        let n = octal_length s (i + 1) in
        let value =
          String.sub s (i + 1) n
          |> String.fold_left
               (fun (value : int) (c : char) ->
                 (value * 8) + Char.Ascii.digit_to_int c)
               0
        in
        code_point_escape i value (i + 1 + n)
    | 'x' -> (
        match hex_value s (i + 2) 2 with
        | Some n -> code_point_escape i n (i + 4)
        | None -> keep i)
    | 'u' when not bytes -> unicode_escape i 4
    | 'U' when not bytes -> unicode_escape i 8
    | _ -> keep i
  and unicode_escape (i : int) (count : int) : unit =
    match hex_value s (i + 2) count with
    | Some n -> code_point_escape i n (i + 2 + count)
    | None -> keep i
  (* The escape at [i], which ends before [next], has the value [n]. In a
     bytes literal, that value is a byte. *)
  and code_point_escape (i : int) (n : int) (next : int) : unit =
    if bytes then
      let byte = n land 0xFF in
      if byte < 0x80 then (
        Buffer.add_char buf (Char.chr byte);
        scan next)
      else keep i
    else if Uchar.is_valid n then (
      Buffer.add_utf_8_uchar buf (Uchar.of_int n);
      scan next)
    else keep i
  and keep (i : int) : unit =
    Buffer.add_char buf '\\';
    Buffer.add_char buf s.[i + 1];
    scan (i + 2)
  in
  scan 0;
  Buffer.contents buf

let prefix_has (letter : char) (prefix : string) : bool =
  String.exists
    (fun (c : char) -> Char.equal (Char.lowercase_ascii c) letter)
    prefix

let is_raw_prefix (prefix : string) : bool = prefix_has 'r' prefix
let is_format_prefix (prefix : string) : bool = prefix_has 'f' prefix

(* In Python 2, a string literal without a u prefix is a byte string, unless
   the module imports unicode_literals from __future__, which is not
   tracked. *)
let is_bytes_literal ~(python2 : bool) (prefix : string) : bool =
  prefix_has 'b' prefix || (python2 && not (prefix_has 'u' prefix))

(* A raw literal has no escape sequences: a \u or \U in a Python 2 ur literal is
   kept as text. In a pattern, "=~/regexp/" is a regular expression, whose
   backslashes belong to the regular expression. *)
let keeps_source_text ~(pattern : bool) ~(prefix : string) (s : string) : bool =
  is_raw_prefix prefix || (pattern && Regexp_string.is_regexp_string s)

let value ~(python2 : bool) ~(pattern : bool) ~(prefix : string) (s : string) :
    string =
  if keeps_source_text ~pattern ~prefix s then s
  else unescaped ~bytes:(is_bytes_literal ~python2 prefix) s
