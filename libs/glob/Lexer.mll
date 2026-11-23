{
module M = Pattern
open Parser

exception Syntax_error of string

let syntax_error msg =
  raise (Syntax_error msg)

(* A range 'a-b' matches 'a' even when 'b' sorts before 'a', and nothing
   else then, as in git. *)
let range (a : char) (b : char) : M.char_class_range =
  if Char.compare a b <= 0 then M.Range (a, b) else M.Class_char a

(* The members of a POSIX character class, in the C locale, except 'space',
   which git limits to space, tab, LF and CR. *)
let posix_class (name : string) : M.char_class_range list =
  match name with
  | "alpha" -> [ M.Range ('a', 'z'); M.Range ('A', 'Z') ]
  | "digit" -> [ M.Range ('0', '9') ]
  | "alnum" -> [ M.Range ('a', 'z'); M.Range ('A', 'Z'); M.Range ('0', '9') ]
  | "upper" -> [ M.Range ('A', 'Z') ]
  | "lower" -> [ M.Range ('a', 'z') ]
  | "xdigit" -> [ M.Range ('0', '9'); M.Range ('a', 'f'); M.Range ('A', 'F') ]
  | "space" ->
      [ M.Class_char ' '; M.Class_char '\t'; M.Class_char '\n';
        M.Class_char '\r' ]
  | "blank" -> [ M.Class_char ' '; M.Class_char '\t' ]
  | "punct" ->
      [ M.Range ('!', '/'); M.Range (':', '@'); M.Range ('[', '`');
        M.Range ('{', '~') ]
  | "print" -> [ M.Range (' ', '~') ]
  | "graph" -> [ M.Range ('!', '~') ]
  | "cntrl" -> [ M.Range ('\000', '\031'); M.Class_char '\127' ]
  | _ -> syntax_error (Printf.sprintf "unknown character class '[:%s:]'" name)
}

let neg = ['^' '!']

rule token = parse
(* each '/' separates two segments: 'a//b' holds an empty segment and
   matches only a path with '//' there, as in git *)
| '/'      { SLASH }
(* three or more stars are '**', as in git *)
| "**" '*'*
           { (* only special if it occupies a whole path segment. This
                is dealt with later. *)
             STARSTAR }
| '*'      { STAR }
| '?'      { QUESTION }
| '[' (neg? as negation)
           {
             let ranges = char_class true [] lexbuf in
             let complement = (negation <> "") in
             CHAR_CLASS { complement; ranges }
           }
| '\\' (_ as c)
           { CHAR c }
| '\\' eof
           { syntax_error "trailing backslash" }
| [^ '/' '*' '?' '[' '\\'] as c
           { CHAR c }
| eof      { EOF }

(* The members of a bracket expression, as in git: a ']' right after '[' or
   the negation is a member, a '\' quotes the next character, '[:' up to
   the next ']' is a POSIX class when that ']' follows a ':' (otherwise the
   '[' is a member), and a '-' before the closing ']' is a member. [first]
   is true before the first member. *)
and char_class first acc = parse
| ']'      { if first then range_or_member ']' acc lexbuf else List.rev acc }
| "[:" ([^ ']']* as name) ":]"
           { char_class false (List.rev_append (posix_class name) acc) lexbuf }
| '\\' (_ as c)
           { range_or_member c acc lexbuf }
| '\\' eof { syntax_error "trailing backslash" }
| eof      { syntax_error "missing ']'" }
| _ as c   { range_or_member c acc lexbuf }

(* After the member [a]: a range 'a-b', unless the '-' comes before the
   closing ']'. *)
and range_or_member a acc = parse
| '-' '\\' (_ as b)
           { char_class false (range a b :: acc) lexbuf }
| '-' ([^ ']' '\\'] as b)
           { char_class false (range a b :: acc) lexbuf }
| ""       { char_class false (M.Class_char a :: acc) lexbuf }
