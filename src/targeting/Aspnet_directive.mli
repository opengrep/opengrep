(*
   The directives that open an ASP.NET generic handler (.ashx) or web
   service (.asmx) file, such as
   '<%@ WebHandler Language="C#" Class="Echo" %>'. The rest of such a file
   is a C# or VB compilation unit.
*)

(* Whether the file is an ASP.NET generic handler or web service, by its
   extension, compared with Filename.check_suffix as the extensions of the
   other languages are: case-insensitive on Windows. *)
val is_handler_file : Fpath.t -> bool

type header = {
  (* The language of the code: the one the Language attribute selects, VB
     without one. [None] for a language opengrep does not parse, such as
     JScript. *)
  language : Lang.t option;
  (* The byte offset just after the last directive. *)
  code_start : int;
}

(* The directives at the start of the text, after an optional byte order
   mark and whitespace. [None] when the text does not start with a
   directive or a directive does not end with '%>'. *)
val scan : string -> header option

(* The text with every byte before [code_start] that is not a line break
   replaced by a space, so that the code keeps its lines and byte columns;
   the text itself when [scan] returns [None]. *)
val blank_header : string -> string
