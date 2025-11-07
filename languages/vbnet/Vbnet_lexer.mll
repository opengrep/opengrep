{
module T = Vbnet_token

(* ===== *)
(* State *)
(* ===== *)

(* NOTE: The VB.NET lexer is stateful in order to implement interpolated
 * strings. The state is a stack, because we need to take into account
 * nested interpolated strings. We also need to count braces to
 * distinguish between '}' that is simply punctuation and '}' that ends
 * an inner expression.
 *)
type lexer_state =
  | Initial
  | InString
  | Brace

let initial_state = [Initial]

let push_state state s =
  state := s :: !state

let pop_state state =
  state :=
    (match !state with
    | _ :: tl -> tl
    | [] -> [Initial])

let current_states state =
  match !state with
  | [] -> [Initial]
  | xs -> xs

let current_state state = current_states state |> List.hd

(* ======================= *)
(* Keywords vs identifiers *)
(* ======================= *)

(* NOTE: VB.NET has a few contextual keywords that has special meaning only in
 * certain contexts, while in others they can be used as identifiers. The latter
 * are commented out below. Because the parser can take into account the actual
 * content of the token, we lex them as identifiers, and distinguish them by
 * the actual context during parsing.
 *)
let kw_or_ident (buf : Lexing.lexbuf) : T.t =
  match String.uppercase_ascii (Lexing.lexeme buf) with
  | "ADDHANDLER"
  | "ADDRESSOF"
  | "ALIAS"
  | "AND"
  | "ANDALSO"
  | "AS"
 (* | "BOOLEAN" *)
  | "BYREF"
 (* | "BYTE" *)
  | "BYVAL"
  | "CALL"
  | "CASE"
  | "CATCH"
  | "CBOOL"
  | "CBYTE"
  | "CCHAR"
  | "CDATE"
  | "CDBL"
  | "CDEC"
 (* | "CHAR" *)
  | "CINT"
  | "CLASS"
  | "CLNG"
  | "COBJ"
  | "CONST"
  | "CONTINUE"
  | "CSBYTE"
  | "CSHORT"
  | "CSNG"
  | "CSTR"
  | "CTYPE"
  | "CUINT"
  | "CULNG"
  | "CUSHORT"
 (* | "DATE" *)
 (* | "DECIMAL" *)
  | "DECLARE"
 (* | "DEFAULT" *)
  | "DELEGATE"
  | "DIM"
  | "DIRECTCAST"
  | "DO"
 (* | "DOUBLE" *)
  | "EACH"
  | "ELSE"
  | "ELSEIF"
  | "END"
  | "ENDIF"
  | "ENUM"
  | "ERASE"
 (* | "ERROR" *)
  | "EVENT"
  | "EXIT"
  | "FALSE"
  | "FINALLY"
  | "FOR"
  | "FRIEND"
  | "FUNCTION"
  | "GET"
 (* | "GETTYPE" *)
  | "GETXMLNAMESPACE"
  | "GOSUB"
  | "GOTO"
  | "HANDLES"
  | "IF"
  | "IMPLEMENTS"
  | "IMPORTS"
  | "IN"
  | "INHERITS"
 (* | "INTEGER" *)
  | "INTERFACE"
  | "IS"
  | "ISNOT"
  | "LET"
  | "LIB"
  | "LIKE"
 (* | "LONG" *)
  | "LOOP"
  | "ME"
  | "MOD"
  | "MODULE"
  | "MUSTINHERIT"
  | "MUSTOVERRIDE"
  | "MYBASE"
  | "MYCLASS"
  | "NAMESPACE"
  | "NARROWING"
  | "NEW"
  | "NEXT"
  | "NOT"
  | "NOTHING"
  | "NOTINHERITABLE"
  | "NOTOVERRIDABLE"
 (* | "OBJECT" *)
  | "OF"
  | "ON"
 (* | "OPERATOR" *)
  | "OPTION"
  | "OPTIONAL"
  | "OR"
  | "ORELSE"
  | "OVERLOADS"
  | "OVERRIDABLE"
  | "OVERRIDES"
  | "PARAMARRAY"
  | "PARTIAL"
  | "PRIVATE"
  | "PROPERTY"
  | "PROTECTED"
  | "PUBLIC"
  | "RAISEEVENT"
 (* | "READONLY" *)
  | "REDIM"
  | "REMOVEHANDLER"
  | "RESUME"
  | "RETURN"
 (* | "SBYTE" *)
  | "SELECT"
  | "SET"
  | "SHADOWS"
  | "SHARED"
 (* | "SHORT" *)
 (* | "SINGLE" *)
  | "STATIC"
  | "STEP"
 (* | "STOP" *)
 (* | "STRING" *)
  | "STRUCTURE"
  | "SUB"
  | "SYNCLOCK"
  | "THEN"
  | "THROW"
 (* | "TO" *)
  | "TRUE"
  | "TRY"
  | "TRYCAST"
  | "TYPEOF"
 (* | "UINTEGER" *)
 (* | "ULONG" *)
 (* | "USHORT" *)
  | "USING"
  | "VARIANT"
  | "WEND"
  | "WHEN"
  | "WHILE"
  | "WIDENING"
  | "WITH"
  | "WITHEVENTS"
  | "WRITEONLY"
  | "XOR"
 (* | "YIELD" *) ->
      T.make ~uppercase:true buf T.Keyword
  | _ ->
      T.make ~uppercase:true buf T.Identifier
}

(* ======================= *)
(* Whitespace and comments *)
(* ======================= *)

let whitespace =
    '\t'                          (* Tab *)
  | ' '                           (* Space *)
  | "\xC2\xA0"                    (* No-Break Space *)
  | "\xE1\x9A\x80"                (* Ogham Space Mark *)
  | ("\xE2\x80" ['\x80'-'\x8A'])  (* En Space to Hair Space *)
  | "\xE2\x80\xAF"                (* Narrow No-Break Space *)
  | "\xE2\x81\x9F"                (* Medium Mathematical Space *)
  | "\xE3\x80\x80"                (* Ideographic Space *)
  | "\xEF\xBB\xBF"                (* UTF-8 BOM *)

let comma =
    "\x2C"                        (* , *)
  | "\xD8\x8C"                    (* U+060C Arabic comma *)
  | "\xD5\x9D"                    (* U+055D Armenian comma *)
  | "\xE3\x80\x81"                (* U+3001 Ideographic comma *)
  | "\xEF\xBC\x8C"                (* U+FF0C Fullwidth comma *)

let line_terminator =
    '\r'                          (* U+000D: Carriage Return *)
  | '\n'                          (* U+000A: Line Feed *)
  | '\r' '\n'                     (* Windows *)
  | "\xE2\x80\xA8"                (* U+2028: Line Separator *)
  | "\xE2\x80\xA9"                (* U+2029: Paragraph Separator *)

let string_char =
    ['\x00'-'\x21' '\x23'-'\x7F']                          (* ASCII, excluding '"' (U+0022) *)
  | ['\xC2'-'\xDF'] ['\x80'-'\xBF']                        (* 2-byte UTF-8 *)
  | "\xE0" ['\xA0'-'\xBF'] ['\x80'-'\xBF']                 (* 3-byte: U+0800–U+0FFF *)
  | ['\xE1'-'\xE2'] ['\x80'-'\xBF'] ['\x80'-'\xBF']        (* 3-byte: U+1000–U+20FF, excluding U+201C/U+201D *)
  | "\xE2" ['\x80'-'\x8B' '\x8E'-'\xBF'] ['\x80'-'\xBF']   (* Skip U+201C (0xE2 0x80 0x9C) and U+201D (0xE2 0x80 0x9D) *)
  | ['\xE3'-'\xEC'] ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | "\xED" ['\x80'-'\x9F'] ['\x80'-'\xBF']
  | ['\xEE'-'\xEF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | "\xF0" ['\x90'-'\xBF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | ['\xF1'-'\xF3'] ['\x80'-'\xBF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']

let not_line_terminator =
    [^'\r' '\n' '\xE2']
  | '\xE2' [^'\x80']
  | "\xE2\x80" [^'\xA8' '\xA9']

(* NOTE: In VB it's incorrect to have a line that contains only a line
 * continuation, like
 *
 * Dim x = 1 + _
 * _
 * 2
 *
 * However, we accept it, as it seems harmless
 *)

let single_quot_char =
    '\''             (* U+0027: ASCII Single Quote *)
  | "\xE2\x80\x98"   (* U+2018: Left Single Quotation Mark *)
  | "\xE2\x80\x99"   (* U+2019: Right Single Quotation Mark *)

let comment_end_of_line =
    ("REM" whitespace | single_quot_char) not_line_terminator* line_terminator
  | "REM" line_terminator

let comment_end_of_file =
    ("REM" whitespace | single_quot_char) not_line_terminator* eof
  | "REM" eof

let line_continuation =
  '_' whitespace* (comment_end_of_line | line_terminator)

let ignored_preproc_directives_end_of_line =
    "#" whitespace* ("If" | "Else" | "Region" | "End" | "ExternalSource" | "Const" | "ExternalChecksum" | "Enable" | "Disable") not_line_terminator* line_terminator

let ignored_preproc_directives_end_of_file =
    "#" whitespace* ("If" | "Else" | "Region" | "End" | "ExternalSource" | "Const" | "ExternalChecksum" | "Enable" | "Disable") not_line_terminator* eof

(* ======================== *)
(* Keywords and identifiers *)
(* ======================== *)

(* FIXME: The language specification defines alpha_character as
 * '<Unicode classes Lu,Ll,Lt,Lm,Lo,Nl>'. Since we cannot specify Unicode
 * classes in ocamllex, we use an approximation. Similarly with other
 * regexps in this section.
 * TODO: Switch to sedlex? *)

let cjk_char =
    "\xE4" ['\xB8'-'\xBF'] ['\x80'-'\xBF']
  | "\xE5" ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | "\xE6" ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | "\xE7" ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | "\xE8" ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | "\xE9" ['\x80'-'\xBF'] ['\x80'-'\xBF']

let kana_char =
    "\xE3" ['\x81'-'\x83'] ['\x80'-'\xBF']
  | "\xE3" ['\x82'-'\x83'] ['\x80'-'\xBF']

let hangul_char =
    "\xEA" ['\xB0'-'\xBF'] ['\x80'-'\xBF']
  | "\xEB" ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | "\xEC" ['\x80'-'\xBF'] ['\x80'-'\xBF']

let alpha_char =
    ['A'-'Z' 'a'-'z']                 (* ASCII Latin letters *)
  | "\xC3" ['\x80'-'\xBF']            (* Latin-1 Supplement *)
  | "\xC4" ['\x80'-'\xBF']            (* Latin Extended-A *)
  | "\xC5" ['\x80'-'\xBF']            (* Latin Extended-A continued *)
  | "\xCE" ['\x91'-'\xBF']            (* Greek and Coptic *)
  | "\xCF" ['\x80'-'\xBF']            (* Greek and Coptic continued *)
  | "\xD0" ['\x80'-'\xBF']            (* Cyrillic *)
  | "\xD1" ['\x80'-'\xBF']            (* Cyrillic continued *)
  | "\xD2" ['\x80'-'\xBF']            (* Cyrillic Extended-A *)
  | "\xD3" ['\x80'-'\xBF']            (* Cyrillic Extended-A continued *)
  | "\xD4" ['\x80'-'\xAF']            (* Armenian: U+0530–U+055F *)
  | "\xD5" ['\x80'-'\xBF']            (* Armenian: U+0560–U+058F *)
  | "\xD7" ['\x90'-'\xAA']            (* Hebrew: U+05D0–U+05EA (letters) *)
  | cjk_char
  | kana_char
  | hangul_char

let numeric_char =
    ['0'-'9']                         (* ASCII digits *)
  | "\xD9" ['\xA0'-'\xA9']            (* Arabic-Indic digits *)
  | "\xDB" ['\xB0'-'\xB9']            (* Extended Arabic-Indic digits *)
  | "\xE0\xA5" ['\xA6'-'\xAF']        (* Devanagari digits *)
  | "\xE0\xA7" ['\xA6'-'\xAF']        (* Bengali digits *)
  | "\xE0\xAF" ['\xA6'-'\xAF']        (* Tamil digits *)

let combining_char =
  (* Mn: Nonspacing Marks *)
    "\xCC" ['\x80'-'\xBF']            (* U+0300–U+033F: Combining diacritical marks *)
  | "\xCD" ['\x80'-'\xBF']            (* U+0340–U+037F: Extended combining marks *)
  (* Mc: Spacing Combining Marks *)
  | "\xE0\xA4" ['\xBE'-'\xBF']        (* U+093E–U+093F: Devanagari vowel signs *)
  | "\xE0\xA5" ['\x80'-'\x89']        (* U+0940–U+0949: More Devanagari vowel signs *)
  | "\xE0\xA7" ['\xBE'-'\xBF']        (* U+09BE–U+09BF: Bengali vowel signs *)
  | "\xE0\xA8" ['\x80'-'\x81']        (* U+0A00–U+0A01: Gurmukhi vowel signs *)

let formatting_char =
    "\xE2\x80\x8B"                    (* U+200B: Zero Width Space *)
  | "\xE2\x80\x8C"                    (* U+200C: Zero Width Non-Joiner *)
  | "\xE2\x80\x8D"                    (* U+200D: Zero Width Joiner *)
  | "\xE2\x80\x8E"                    (* U+200E: Left-to-Right Mark *)
  | "\xE2\x80\x8F"                    (* U+200F: Right-to-Left Mark *)
  | "\xEF\xBB\xBF"                    (* U+FEFF: Byte Order Mark *)

let underscore_char =
    "_"                               (* U+005F: ASCII underscore *)
  | "\xE2\x80\x85"                    (* U+2005: Four-per-em space — sometimes used as connector *)
  | "\xE2\x81\xA0"                    (* U+2060: Word Joiner *)
  | "\xE2\x88\x92"                    (* U+2212: Minus sign — visually similar to underscore in some fonts *)
  | "\xEF\xB8\x80"                    (* U+FE80: Arabic letter hamza isolated form — used in some identifier schemes *)
  | "\xEF\xB8\x8F"                    (* U+FE8F: Arabic letter alef isolated form — connector in some contexts *)
  | "\xEF\xBC\xBF"                    (* U+FF3F: Fullwidth underscore — used in East Asian typography *)

let type_char =
  ['%' '&' '@' '!' '#' '$']

(* ======== *)
(* Literals *)
(* ======== *)

let digit = ['0'-'9']

let hex_digit = ['0'-'9' 'A'-'F']

let octal_digit = ['0'-'7']

let binary_digit = ['0'-'1']

let integral_type_char = "S" | "US" | "I" | "UI" | "L" | "UL" | "D" | type_char

let sign = ['+' '-']

let exponent_char = ['E' 'e']

let floating_point_type_char = ['F' 'f' 'D' 'd' 'R' 'r' '#' '!' '@']

let double_quote_char =
    '"'                                (* U+0022: ASCII double quote *)
  | "\xE2\x80\x9C"                     (* U+201C: Left double quotation mark *)
  | "\xE2\x80\x9D"                     (* U+201D: Right double quotation mark *)

let string_char =
  [^'\x22''\xE2'] | "\xE2" [^'\x80'] | "\xE2\x80" [^'\x9C''\x9D']

let interpolated_string_char =
    ['\x00'-'\x21' '\x23'-'\x7A' '\x7C'-'\x7F']            (* ASCII, excluding '"' and '{' *)
  | ['\xC2'-'\xDF'] ['\x80'-'\xBF']                        (* 2-byte UTF-8 *)
  | "\xE0" ['\xA0'-'\xBF'] ['\x80'-'\xBF']                 (* 3-byte: U+0800–U+0FFF *)
  | ['\xE1'-'\xE2'] ['\x80'-'\xBF'] ['\x80'-'\xBF']        (* 3-byte: U+1000–U+20FF, excluding U+201C/U+201D *)
  | "\xE2" ['\x80'-'\x8B' '\x8E'-'\xBF'] ['\x80'-'\xBF']   (* Skip U+201C (0xE2 0x80 0x9C) and U+201D (0xE2 0x80 0x9D) *)
  | ['\xE3'-'\xEC'] ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | "\xED" ['\x80'-'\x9F'] ['\x80'-'\xBF']
  | ['\xEE'-'\xEF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | "\xF0" ['\x90'-'\xBF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | ['\xF1'-'\xF3'] ['\x80'-'\xBF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']
  | "\xF4" ['\x80'-'\x8F'] ['\x80'-'\xBF'] ['\x80'-'\xBF']

let date_value =
  ['0'-'9']+ ['/' '-'] ['0'-'9']+ ['/' '-'] ['0'-'9']+

let time_value =
    ['0'-'9']+ ':' ['0'-'9']+ (':' ['0'-'9']+)? [' ' '\t']* ("am" | "AM" | "Am" | "aM" | "pm" | "PM" | "Pm" | "pM")?
  | ['0'-'9']+ [' ' '\t']* ("am" | "AM" | "Am" | "aM" | "pm" | "PM" | "Pm" | "pM")

(* ===== *)
(* Lexer *)
(* ===== *)

rule token state = parse
  | "" {
       match current_state state with
       | Initial | Brace -> read state lexbuf
       | InString -> lex_in_string state lexbuf
       }

and read state = parse

(* whitespace and comments*)
  | whitespace { read state lexbuf }
  | comment_end_of_line { T.make lexbuf T.LineTerminator }
  | comment_end_of_file { T.make lexbuf T.EOF }
  | line_continuation { read state lexbuf }
  | line_terminator { T.make lexbuf T.LineTerminator }

(* ignored preprocessing directives *)
  | ignored_preproc_directives_end_of_line { T.make lexbuf T.LineTerminator }
  | ignored_preproc_directives_end_of_file { T.make lexbuf T.LineTerminator }
  | "#Else" [^'#']* "#End If" { T.make lexbuf T.LineTerminator }

(* XML CDATA *)
  | "<![CDATA[" (([^']'] | ']' [^']'] | "]]" [^'>'] )* as s) "]]>" { T.make lexbuf (T.CDATA s) }

(* keywords and identifiers *)
  | (alpha_char | underscore_char (alpha_char | numeric_char | combining_char | formatting_char | underscore_char))
    (alpha_char | numeric_char | combining_char | formatting_char | underscore_char)*
    type_char?
    { kw_or_ident lexbuf }
  | '[' ((alpha_char | numeric_char | combining_char | formatting_char | underscore_char)+) ']'
    { T.make ~uppercase:true lexbuf T.Identifier }
    (* some special cases *)
  | ("Mid$" | "Left$" | "Right$" | "RCase$" | "Trim$" | "UCase$")
    { T.make ~uppercase:true lexbuf T.Identifier }
    (* opengrep metavar *)
  | '$' ['A'-'Z']['A'-'Z' '0'-'9']* { T.make lexbuf T.Identifier }

(* interpolated strings *)
  | "$\"" {
      push_state state InString;
      T.make lexbuf T.Operator
    }

(* literals *)
  | ((digit (digit | '_')*) as s) integral_type_char?
    { let n = Int64.of_string s in
      T.make lexbuf (T.IntLiteral n) }
  | '&' ('H' | 'h') ((hex_digit (hex_digit | '_')*) as s) integral_type_char?
    { let n = Int64.of_string ("0x" ^ s) in
      T.make lexbuf (T.IntLiteral n) }
  | '&' ('O' | 'o') (octal_digit (octal_digit | '_')* as s) integral_type_char?
    { let n = Int64.of_string ("0o" ^ s) in
      T.make lexbuf (T.IntLiteral n) }
  | '&' ('B' | 'b') (binary_digit (binary_digit | '_')* as s) integral_type_char?
    { let n = Int64.of_string ("0b" ^ s) in
      T.make lexbuf (T.IntLiteral n) }
  | digit+ '.' digit+ (exponent_char sign digit+)? floating_point_type_char?
    { T.make lexbuf T.FloatLiteral }
  | '.' digit+ (exponent_char sign digit+)? floating_point_type_char?
    { T.make lexbuf T.FloatLiteral }
  | digit+ exponent_char sign digit+ floating_point_type_char?
    { T.make lexbuf T.FloatLiteral }
  | digit+ floating_point_type_char
    { T.make lexbuf T.FloatLiteral }
  | double_quote_char ((string_char | double_quote_char double_quote_char)* as s) double_quote_char
    { T.make lexbuf (T.StringLiteral s) }
  | double_quote_char ((string_char | double_quote_char double_quote_char)* as s) double_quote_char ('c' | 'C')
    { T.make lexbuf (T.CharLiteral s) }
  | '#' whitespace* (date_value | date_value whitespace+ time_value | time_value) whitespace* '#'
    { T.make lexbuf T.DateLiteral }

(* punctuation *)
  | "(" { T.make lexbuf T.Punctuation }
  | ")" { T.make lexbuf T.Punctuation }
  | "{" { push_state state Brace; T.make lexbuf T.Punctuation }
  | "}" {
      match current_states state with
      | Brace :: _ -> (pop_state state; T.make lexbuf T.Punctuation)
      | Initial :: InString :: _ -> (pop_state state;
                                     token state lexbuf)
      | _ -> T.make lexbuf T.Punctuation
      }
  | comma { let x = T.make lexbuf T.Punctuation in { x with content = "," } }

(* operators *)
  | "!" { T.make lexbuf T.Operator }
  | "\"" { T.make lexbuf T.Operator }
  | "#" { T.make lexbuf T.Operator }
  | "$\"" { T.make lexbuf T.Operator }
  (*| "$" { T.make lexbuf T.Operator }*)
  | "%>" { T.make lexbuf T.Operator }
  | "&" { T.make lexbuf T.Operator }
  | "&=" { T.make lexbuf T.Operator }
  | "*" { T.make lexbuf T.Operator }
  | "*=" { T.make lexbuf T.Operator }
  | "+" { T.make lexbuf T.Operator }
  | "+=" { T.make lexbuf T.Operator }
  | "-" { T.make lexbuf T.Operator }
  | "-->" { T.make lexbuf T.Operator }
  | "-=" { T.make lexbuf T.Operator }
  | "." { T.make lexbuf T.Operator }
  | "/" { T.make lexbuf T.Operator }
  | "/=" { T.make lexbuf T.Operator }
  | "/>" { T.make lexbuf T.Operator }
  | ":" { T.make lexbuf T.Operator }
  | ":=" { T.make lexbuf T.Operator }
  | "<" { T.make lexbuf T.Operator }
  | "<!--" { T.make lexbuf T.Operator }
  | "<![CDATA[" { T.make lexbuf T.Operator }
  | "<%=" { T.make lexbuf T.Operator }
  | "</" { T.make lexbuf T.Operator }
  | "<<" { T.make lexbuf T.Operator }
  | "<<=" { T.make lexbuf T.Operator }
  | "<=" { T.make lexbuf T.Operator }
  | "<>" { T.make lexbuf T.Operator }
  | "<?" { T.make lexbuf T.Operator }
  | "=" { T.make lexbuf T.Operator }
  | ">" { T.make lexbuf T.Operator }
  | ">=" { T.make lexbuf T.Operator }
  | ">>" { T.make lexbuf T.Operator }
  | ">>=" { T.make lexbuf T.Operator }
  | "?" { T.make lexbuf T.Operator }
  | "?." { T.make lexbuf T.Operator }
  | "?>" { T.make lexbuf T.Operator }
  | "@" { T.make lexbuf T.Operator }
  | ".@" { T.make lexbuf T.Operator }
  | ".@<" { T.make lexbuf T.Operator }
  | "\\" { T.make lexbuf T.Operator }
  | "\\=" { T.make lexbuf T.Operator }
  | "]]>" { T.make lexbuf T.Operator }
  | "^" { T.make lexbuf T.Operator }
  | "^=" { T.make lexbuf T.Operator }
  | ";" { T.make lexbuf T.Operator }
  | "|" { T.make lexbuf T.Operator }
  | "..." { T.make lexbuf T.Operator }
  | "<..." { T.make lexbuf T.Operator }
  | "...>" { T.make lexbuf T.Operator }

(* other *)
  | eof  { T.make lexbuf T.EOF; }

(* fallback -- useful for xml *)
  | _ { T.make lexbuf T.Other }

and lex_in_string state = parse
  | (interpolated_string_char | "{{" | double_quote_char double_quote_char)+
    {T.make lexbuf T.StringSegment}
  | '"' { pop_state state; T.make lexbuf T.Operator }
  | '{' { push_state state Initial; token state lexbuf }
