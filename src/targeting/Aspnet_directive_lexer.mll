{
(* Tokens of the directives that open an ASP.NET handler or web service
   file. The character classes follow the directive syntax of System.Web
   (DirectiveRegex), over ASCII. *)
type token =
  | Byte_order_mark
  | Whitespace
  | Directive_start
  | Directive_end
  | Attribute_name of string
  | Equal
  | Other
  | Eof
}

let whitespace = [' ' '\t' '\r' '\n' '\011' '\012']
let word = ['a'-'z' 'A'-'Z' '0'-'9' '_']

rule token = parse
  | "\xEF\xBB\xBF" { Byte_order_mark }
  | whitespace+ { Whitespace }
  | "<%" whitespace* '@' { Directive_start }
  | "%>" { Directive_end }
  | word (word | ':')* as name { Attribute_name name }
  | '=' { Equal }
  | eof { Eof }
  | _ { Other }

(* The value after '=': quoted, which may contain "%>", or unquoted, which
   may be empty. *)
and value = parse
  | whitespace* '"' ([^ '"']* as quoted) '"' { quoted }
  | whitespace* '\'' ([^ '\'']* as quoted) '\'' { quoted }
  | whitespace* ([^ ' ' '\t' '\r' '\n' '\011' '\012' '"' '\'' '%' '>']* as unquoted)
      { unquoted }
