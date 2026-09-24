type 'a env = {
  file : Fpath.t;
  (* Get the byte offset (0-based) in the source from a (line, column) pair.
     The line is 1-based, the column is 0-based.
     Raises Not_found.
     TODO: rename 'bytepos_of_line_col'
     TODO: return an option instead of raising an exception *)
  conv : int * int -> int;
  extra : 'a;
}

(* Take a file, return a lookup function that may raise Not_found.
   This is used to populate the field 'conv' above.
   TODO: rename 'bytepos_of_line_col' *)
val line_col_to_pos : Fpath.t -> int * int -> int

(* Take a string, return a lookup function that may raise Not_found.
   TODO: rename 'bytepos_of_line_col'
   TODO: explain what's special about patterns. It seems like it should
   work for any input string.
*)
val line_col_to_pos_pattern : string (* contents *) -> int * int -> int

(* Tree_sitter_run tokens to Tok.t converters *)
val token : 'a env -> Tree_sitter_run.Token.t -> Tok.t
val str : 'a env -> Tree_sitter_run.Token.t -> string * Tok.t

(* Row first, then column. *)
val compare_pos : Tree_sitter_run.Loc.pos -> Tree_sitter_run.Loc.pos -> int
val equal_loc : Tree_sitter_run.Loc.t -> Tree_sitter_run.Loc.t -> bool

(* Heredocs, shared by Ruby and Crystal. A heredoc is a marker such as
   "<<-SQL" where the string goes, and a body among the extras of the parse,
   from the end of the marker line to a terminator line. *)

(* The heredoc markers of a raw tree. [constructor] names the token case that
   holds one: "Here_begin" in Ruby, "Here_start" in Crystal. *)
val heredoc_markers :
  constructor:string -> _ Tree_sitter_run.Raw_tree.t -> Tree_sitter_run.Token.t list

(* Each marker with its body. The markers are taken in source order, and each
   takes the first body left after it that its delimiter closes: the bodies of
   the markers of a line follow that line, in the same order. [delimiter] reads
   the delimiter off a marker, "SQL" off "<<-'SQL'"; [terminator] reads the
   terminator text off a body. An empty terminator ends a file that has no
   last newline, where the scanner does not close the heredoc, and closes the
   marker at hand. A marker whose body is not found is left out. *)
val pair_heredocs :
  delimiter:(string -> string) ->
  terminator:('body -> string) ->
  Tree_sitter_run.Token.t list ->
  (Tree_sitter_run.Loc.t * 'body) list ->
  (Tree_sitter_run.Loc.t * 'body) list

(* The body paired with the marker at this location. *)
val heredoc_body :
  Tree_sitter_run.Loc.t -> (Tree_sitter_run.Loc.t * 'body) list -> 'body option

(* [dedent n text] removes up to [n] blanks and tabs after each newline. *)
val dedent : int -> string -> string

(* tree-sitter starts a body at the end of the marker line, but the newline
   ending that line is not part of the string: the leading newline of a first
   text part is dropped, and the part with it when nothing else is left. The
   location of the part moves to the next line. [text_part] reads a text part
   as its token and the string it stands for, [make_text_part] builds one
   back; a first part that is not a text is left alone. *)
val drop_marker_newline :
  text_part:('part -> (Tree_sitter_run.Token.t * string) option) ->
  make_text_part:(Tree_sitter_run.Token.t -> string -> 'part) ->
  'part list ->
  'part list

val debug_sexp_cst_after_error : Sexplib.Sexp.t -> unit

(*
   Call a tree-sitter parser and then map the CST into an AST
   with the user-provided function. Takes care of error handling but lets
   exceptions go through.

   Extras (e.g. comments, heredoc bodies, ...) must be injected into the AST
   at this stage.
*)
val wrap_parser :
  (unit -> ('cst, 'extra) Tree_sitter_run.Parsing_result.t) ->
  ('cst -> 'extra list -> 'ast) ->
  ('ast, unit) Tree_sitter_run.Parsing_result.t
