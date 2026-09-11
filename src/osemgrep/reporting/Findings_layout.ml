(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The layout machinery of the text report.
 *
 * Wrapping, tab expansion and dedenting a snippet are the same job whatever
 * a skin decides the report should look like, so they live here rather than
 * in any one skin. So does the classification of a match, which is a fact
 * about the match and not a matter of taste; the titles and the order of the
 * groups are the skin's business.
 *
 * Split out of Matches_report.ml, which is now the legacy skin's renderer.
 *)

module OutJ = Semgrep_output_v1_t
module Log = Log_reporting.Log

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rule_leading_indent_size = 3

let rule_indent_size =
  rule_leading_indent_size + 4 (* severity icon and 1 for space *)

let detail_indent_size = 10
let findings_indent_size = 12
let rule_leading_indent = String.make rule_leading_indent_size ' '
let detail_indent = String.make detail_indent_size ' '
let findings_indent = String.make findings_indent_size ' '

(* python: console.py, the width of the console the wrapper printed on: the
   COLUMNS environment variable when it holds a positive integer, else the
   width of the terminal, at most 120 columns and at least 40 *)
let text_width =
  let max_text_width = 120 and min_text_width = 40 in
  let columns_env : int option =
    match
      Opengrep_env.getenv_opt "COLUMNS"
      |> Option.map String.trim
      |> Fun.flip Option.bind int_of_string_opt
    with
    | Some w when w > 0 -> Some w
    | _ -> None
  in
  let columns : int option =
    match columns_env with
    | Some _ -> columns_env
    | None -> Terminal_size.get_columns ()
  in
  Option.fold columns ~none:max_text_width ~some:(fun (w : int) ->
      Int.min max_text_width (Int.max min_text_width w))

(* python: text.py, the columns rich adds to every line of the findings
   block, and the widths derived from the width of the console:
   FINDINGS_TEXT_WIDTH for a line of code, RULE_TEXT_WIDTH for a rule id,
   DESC_TEXT_WIDTH for a message and AUTOFIX_TEXT_WIDTH for a fix *)
let console_indent_size = 2
let findings_text_width = text_width - 16
let rule_text_width = text_width - 9
let desc_text_width = text_width - 12
let autofix_text_width = text_width - 25

(* python: text.py safe_width(), which keeps a width usable *)
let min_wrap_width = 10
let safe_width (width : int) : int = max min_wrap_width width

(* TODO: re-enable dynamic size in a separate PR to avoid too many test changes *)
let fill_count = 40

(* a rule's metadata is any JSON; a value that is not an object has no
 * members *)
let metadata_member (key : string) (metadata : Yojson.Basic.t) :
    Yojson.Basic.t =
  match metadata with
  | `Assoc _ -> Yojson.Basic.Util.member key metadata
  | _else_ -> `Null

(* like pyopengrep rule_match.py: a match with no "dev.semgrep.actions"
 * metadata is blocking *)
let is_blocking (json : Yojson.Basic.t) =
  match metadata_member "dev.semgrep.actions" json with
  | `List actions ->
      actions
      |> List.exists (function
           | `String action -> String.equal action "block"
           | _else -> false)
  (* the scalar form: dev.semgrep.actions: block *)
  | `String action -> String.equal action "block"
  | `Null -> true
  | _else -> false

let ws_prefix s =
  let rec index_rec s lim i acc =
    if i >= lim then List.rev acc
    else
      let c = s.[i] in
      if c = ' ' then index_rec s lim (i + 1) (' ' :: acc)
      else if c = '\t' then index_rec s lim (i + 1) ('\t' :: acc)
      else List.rev acc
  in
  index_rec s (String.length s) 0 []

let dedent_lines (lines : string list) =
  let ws_prefixes =
    List.sort compare
      (List_.filter_map
         (fun line ->
           if String.(length (trim line)) = 0 then None
           else Some (ws_prefix line))
         lines)
  in
  let longest_prefix =
    let hd, tl =
      match (ws_prefixes, List.rev ws_prefixes) with
      | hd :: _, tl :: _ -> (hd, tl)
      | [], _whatever
      | _whatever, [] ->
          ([], [])
    in
    let rec eq a b togo acc =
      if togo = 0 then acc
      else
        match (a, b) with
        | hda :: tla, hdb :: tlb ->
            if hda = hdb then eq tla tlb (togo - 1) (acc + 1) else acc
        | [], _whatever
        | _whatever, [] ->
            acc
    in
    eq hd tl (min (List.length hd) (List.length tl)) 0
  in
  ( List_.map
      (fun line ->
        if String.(length (trim line)) = 0 then line
        else Str.string_after line longest_prefix)
      lines,
    longest_prefix )

(* python: the width of a tab stop, str.expandtabs()'s default *)
let tab_size = 8

(* python: str.expandtabs(), which TextWrapper runs on the whole text
   before it wraps: a tab moves to the next multiple of eight columns,
   counted from the start of the text. The array returned maps every byte
   offset of [s] to its offset in the result. *)
let expand_tabs (s : string) : string * int array =
  let len = String.length s in
  let buf = Buffer.create len in
  let offsets = Array.make (len + 1) 0 in
  (* a column is a code point, as it is for a Python string *)
  let rec go (i : int) (column : int) : unit =
    if i >= len then ()
    else begin
      offsets.(i) <- Buffer.length buf;
      match s.[i] with
      | '\t' ->
          let pad = tab_size - (column mod tab_size) in
          Buffer.add_string buf (String.make pad ' ');
          go (i + 1) (column + pad)
      | '\n'
      | '\r' ->
          Buffer.add_char buf s.[i];
          go (i + 1) 0
      | (_ : char) ->
          let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
          Buffer.add_string buf (String.sub s i n);
          (* the bytes of a code point are copied, so they keep their
             places within it *)
          for k = 1 to n - 1 do
            offsets.(i + k) <- Buffer.length buf - (n - k)
          done;
          go (i + n) (column + 1)
    end
  in
  go 0 0;
  offsets.(len) <- Buffer.length buf;
  (Buffer.contents buf, offsets)

(* python: TextWrapper._munge_whitespace, the tab expansion followed by a
   space for each of the vertical tab, form feed and carriage return, so
   that the wrapping never sees them. A newline never reaches here: the
   callers split on it first. The translation keeps the byte offsets, so
   the array of expand_tabs still maps the text given here to the result. *)
let munge_whitespace_with_offsets (s : string) : string * int array =
  let expanded, offsets = expand_tabs s in
  ( String.map
      (fun (c : char) ->
        match c with
        | '\011'
        | '\012'
        | '\r' ->
            ' '
        | (_ : char) -> c)
      expanded,
    offsets )

let munge_whitespace (s : string) : string =
  fst (munge_whitespace_with_offsets s)

(* the indentation of the [i]th line of a filled text, after the columns
   the console adds *)
let chunk_indentation ~(initial_indent : int) ~(subsequent_indent : int)
    (i : int) : string =
  String.make
    (console_indent_size + if i = 0 then initial_indent else subsequent_indent)
    ' '

(* python: the two fillers a finding went through. click.wrap_text
   overrides TextWrapper._handle_long_word: a word too long for a line is
   cut at the width, never after a hyphen, and always keeps one column. *)
type filler =
  | Textwrap
  | Click

(*
   Take a piece of text and break it into the lines of a filled paragraph,
   given as (offset, length) pairs in bytes so that the caller can print
   each line with its own indentation and style a range of the text.

   width: maximum space for a line, its indentation included
   initial_indent: number of spaces before the first line
   subsequent_indent: number of spaces before the other lines

   The cuts are made between code points, never inside a UTF-8 sequence:
   at the last space that fits, at the last hyphen between two letters, on
   either side of a run of two or more hyphens between two words, or
   at the width for a word too long for a line of its own. In some context
   (e.g., pre-commit in CI), the number of columns of your terminal can be
   small, in which case the space left for the text can become negative and
   a line then holds a single code point.

   python: textwrap.TextWrapper for a code line, a rule title and the
   autofix; click.wrap_text, which subclasses it, for the rule message.
   The text must already have gone through munge_whitespace.

   The wrapping is covered by the text output tests of
   Test_scan_subcommand_text.ml.
*)
let fill_chunks ~(filler : filler) ~(width : int) ~(initial_indent : int)
    ~(subsequent_indent : int) (s : string) : (int * int) list =
  let offsets = Utf8.code_point_offsets s in
  let n = Array.length offsets - 1 in
  let char_at (i : int) : char = s.[offsets.(i)] in
  let is_space (i : int) : bool = i < n && Char.equal (char_at i) ' ' in
  (* python: '\w' minus the digits, so that "foo-bar" is two chunks but
     "aaa1-2" is one; a code point outside ASCII counts as a letter *)
  let is_letter (i : int) : bool =
    match char_at i with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '_' ->
        true
    | c -> Char.code c >= 0x80
  in
  (* python: wordsep_re, whose hyphenated-word alternative ends a chunk on
     a hyphen preceded by '<letter><letter>' or by '<letter>-<letter>' and
     followed by '<letter>[-]<letter>' *)
  let ends_chunk (i : int) : bool =
    let before =
      (i >= 2 && is_letter (i - 1) && is_letter (i - 2))
      || i >= 3
         && is_letter (i - 1)
         && Char.equal (char_at (i - 2)) '-'
         && is_letter (i - 3)
    in
    let after =
      i + 1 < n && is_letter (i + 1)
      && ((i + 2 < n && is_letter (i + 2))
         || i + 3 < n
            && Char.equal (char_at (i + 2)) '-'
            && is_letter (i + 3))
    in
    Char.equal (char_at i) '-' && before && after
  in
  (* python: '\w' *)
  let is_word (i : int) : bool =
    is_letter i
    ||
    match char_at i with
    | '0' .. '9' -> true
    | (_ : char) -> false
  in
  (* python: word_punct, the class the em-dash alternatives require before
     the hyphens *)
  let is_word_punct (i : int) : bool =
    is_word i
    ||
    match char_at i with
    | '!'
    | '"'
    | '\''
    | '&'
    | '.'
    | ','
    | '?' ->
        true
    | (_ : char) -> false
  in
  (* python: the '-{2,}\w' that both em-dash alternatives of wordsep_re look
     for; the end of the run of hyphens, or None when [i] does not start one
     followed by a word character *)
  let em_dash_run (i : int) : int option =
    let rec run (k : int) : int =
      if k < n && Char.equal (char_at k) '-' then run (k + 1) else k
    in
    let stop = run i in
    if stop - i >= 2 && stop < n && is_word stop then Some stop else None
  in
  (* python: the em-dash alternative of wordsep_re, which makes a run of at
     least two hyphens between two words a chunk of its own *)
  let em_dash_chunk (i : int) : int option =
    if i > 0 && is_word_punct (i - 1) then em_dash_run i else None
  in
  let chunks : (int * int) list =
    let rec spaces (i : int) : int = if is_space i then spaces (i + 1) else i in
    (* a word ends at a space, after a hyphen between two letters, or
       before the run of hyphens that starts the next chunk *)
    let rec word (start : int) (i : int) : int =
      if i >= n || is_space i then i
      else if ends_chunk i then i + 1
      else if i > start && Option.is_some (em_dash_chunk i) then i
      else word start (i + 1)
    in
    let rec go (i : int) (acc : (int * int) list) : (int * int) list =
      if i >= n then List.rev acc
      else
        let j =
          if is_space i then spaces i
          else
            match em_dash_chunk i with
            | Some stop -> stop
            | None -> word i i
        in
        go j ((i, j) :: acc)
    in
    go 0 []
  in
  (* python: TextWrapper._wrap_chunks *)
  let rec fill (chunks : (int * int) list) (lines : (int * int) list) :
      (int * int) list =
    let first = List_.null lines in
    let avail =
      width - if first then initial_indent else subsequent_indent
    in
    (* python: the spaces at the start of a line other than the first are
       dropped *)
    let chunks =
      match chunks with
      | (i, _) :: rest when (not first) && is_space i -> rest
      | _else_ -> chunks
    in
    match chunks with
    | [] -> List.rev lines
    | (start, _) :: _ ->
        (* the chunks that fit, the text being contiguous from [start] *)
        let rec take (cs : (int * int) list) (stop : int) =
          match cs with
          | (_, j) :: rest when j - start <= avail -> take rest j
          | _else_ -> (cs, stop)
        in
        let rest, stop = take chunks start in
        (* python: a chunk too long for a line of its own fills the end of
           this one, after its last hyphen when it has one *)
        let rest, stop =
          match rest with
          | (i, j) :: more when j - i > avail ->
              let space_left =
                match filler with
                | Textwrap ->
                    if avail < 1 then 1 else avail - (stop - start)
                (* python: click's _handle_long_word leaves one column *)
                | Click -> Int.max (avail - (stop - start)) 1
              in
              let cut =
                let plain = min j (stop + space_left) in
                (* the hyphen must have a character of its own before it:
                   the first one after the leading hyphens of the word *)
                let rec first_non_hyphen (p : int) : int =
                  if p < j && Char.equal (char_at p) '-' then
                    first_non_hyphen (p + 1)
                  else p
                in
                let non_hyphen = first_non_hyphen i in
                let rec last_hyphen (k : int) : int option =
                  if k - 1 <= non_hyphen then None
                  else if Char.equal (char_at (k - 1)) '-' then Some k
                  else last_hyphen (k - 1)
                in
                match filler with
                (* python: click's _handle_long_word does not look for a
                   hyphen; it cuts at the width *)
                | Click -> plain
                | Textwrap -> (
                    match last_hyphen plain with
                    | Some k -> k
                    | None -> plain)
              in
              ((if cut >= j then more else (cut, j) :: more), cut)
          | _else_ -> (rest, stop)
        in
        (* python: the spaces at the end of a line are dropped *)
        let rec trim (stop : int) : int =
          if stop > start && is_space (stop - 1) then trim (stop - 1) else stop
        in
        let stop = trim stop in
        fill rest (if stop > start then (start, stop) :: lines else lines)
  in
  fill chunks []
  |> List_.map (fun ((start : int), (stop : int)) ->
         (offsets.(start), offsets.(stop) - offsets.(start)))

(* The lines of [txt] wrapped as [fill_chunks] does, each with the spaces
   to print it after: the indentation of the paragraph plus the two columns
   rich added to every line the wrapper printed. *)
let wrap_lines ~(filler : filler) ~(width : int) ~(initial_indent : int)
    ~(subsequent_indent : int) (txt : string) : (string * string) list =
  Log.debug (fun m ->
      m "wrap width=%d initial_indent=%d subsequent_indent=%d s=%s" width
        initial_indent subsequent_indent txt);
  let txt = munge_whitespace txt in
  let indentation = chunk_indentation ~initial_indent ~subsequent_indent in
  match fill_chunks ~filler ~width ~initial_indent ~subsequent_indent txt with
  | [] -> [ (indentation 0, "") ]
  | chunks ->
      chunks
      |> List.mapi (fun (i : int) ((offset : int), (length : int)) ->
             (indentation i, String.sub txt offset length))

(* The paragraphs of a rule message, each with the indentation of its own
   first line: the lines of a paragraph are joined and filled as one, and
   a blank line separates two paragraphs.

   python: click.wrap_text(preserve_paragraphs=True) in text.py *)
let message_paragraphs (msg : string) : (int * string) list =
  let flush (indent : int option) (buf : string list)
      (acc : (int * string) list) : (int * string) list =
    match buf with
    | [] -> acc
    | _ :: _ ->
        (Option.value ~default:0 indent, buf |> List.rev |> String.concat " ")
        :: acc
  in
  let rec go (lines : string list) (indent : int option) (buf : string list)
      (acc : (int * string) list) : (int * string) list =
    match lines with
    | [] -> List.rev (flush indent buf acc)
    | line :: rest ->
        if String.equal line "" then go rest None [] (flush indent buf acc)
        else
          let indent, line =
            match indent with
            | Some _ -> (indent, line)
            | None ->
                (* python: the paragraph is indented like its first line *)
                let rec first_char (i : int) : int =
                  if i < String.length line && Char.equal line.[i] ' ' then
                    first_char (i + 1)
                  else i
                in
                let i = first_char 0 in
                (Some i, Str.string_after line i)
          in
          go rest indent (line :: buf) acc
  in
  match go (String.split_on_char '\n' msg) None [] [] with
  | [] -> [ (0, "") ]
  | paragraphs -> paragraphs

let cut s idx1 idx2 =
  Log.debug (fun m -> m "cut %d (idx1 %d idx2 %d)" (String.length s) idx1 idx2);
  ( Str.first_chars s idx1,
    String.sub s idx1 (idx2 - idx1),
    Str.string_after s idx2 )

(* python: text.py format_finding_line(), which wraps the number of a line
   and the line itself as one piece of text: 8 columns of indentation, the
   line number right-aligned in 5 columns with its separator, then the
   code, the wrapped lines being indented by 13 columns. *)
let line_number_indent_size = 8
let code_indent_size = 13
let line_number_width = 5

(* A line of code prefixed with its number, wrapped, with the bold part
   [bold_start, bold_end) of the code carried across the wrapped chunks.

   The gutter is described by [number_indent] (the columns before the
   number), [number_width] (the columns the number and its separator are
   right-aligned in) and [code_indent] (where a wrapped chunk resumes,
   normally number_indent + number_width). The defaults are the legacy
   skin's; another skin passes its own. *)
let pp_wrapped_code_line ?(number_indent = line_number_indent_size)
    ?(code_indent = code_indent_size) ?(number_width = line_number_width)
    ?(separator = "┆ ") ppf ~(line_number : int) ~(width : int)
    ~(bold_start : int) ~(bold_end : int) (line : string) : unit =
  let prefix =
    (* python: f"{line_number}┆ ".rjust(5); the separator is one column of
       three bytes, so the padding counts code points *)
    let text = string_of_int line_number ^ separator in
    let columns = Utf8.length text in
    String.make (max 0 (number_width - columns)) ' ' ^ text
  in
  let typed = prefix ^ line in
  let shift = String.length prefix in
  (* the tabs are expanded from the start of the line, its number
     included, so the bold range moves with the text *)
  let text, offset_of = munge_whitespace_with_offsets typed in
  let moved (i : int) : int =
    offset_of.(max 0 (min (String.length typed) (i + shift)))
  in
  let bold_start = moved bold_start and bold_end = moved bold_end in
  let indentation =
    chunk_indentation ~initial_indent:number_indent
      ~subsequent_indent:code_indent
  in
  fill_chunks ~filler:Textwrap ~width ~initial_indent:number_indent
    ~subsequent_indent:code_indent text
  |> List.iteri (fun (i : int) ((offset : int), (length : int)) ->
         let chunk = String.sub text offset length in
         let bold_from = max 0 (min length (bold_start - offset)) in
         let bold_to = max bold_from (min length (bold_end - offset)) in
         let a, b, c = cut chunk bold_from bold_to in
         Fmt.pf ppf "%s%s%a%s@." (indentation i) a
           Fmt.(styled `Bold string)
           b c)

(* The replacement of an autofix, as the lines a skin has to prefix one by
   one. The lines between the first and the last are kept, since a multi-line
   fix is code and reads as such; an empty result means the fix deletes the
   match.

   A fix is spliced into the middle of an existing line, so its first line
   carries no indentation of its own while the lines below it are absolute.
   Giving the first line the column the match starts at ([first_col], as the
   output counts columns, from 1) makes the block whole again, and dedenting
   it then leaves the fix indented relatively to itself. *)
let fix_lines ?(first_col = 1) (fix : string) : string list =
  let rec drop_blank = function
    | (s : string) :: tl when String.equal (String.trim s) "" -> drop_blank tl
    | lines -> lines
  in
  match
    String.split_on_char '\n' fix |> drop_blank |> List.rev |> drop_blank
    |> List.rev
  with
  | [] -> []
  | first :: rest ->
      let first = String.make (max 0 (first_col - 1)) ' ' ^ first in
      fst (dedent_lines (first :: rest))

(*****************************************************************************)
(* Taint traces *)
(*****************************************************************************)

(* A trace as a report states it: a sequence of steps, each a label and the
   lines it points at. Flattening the nested call trace into that sequence is
   a fact about the trace rather than a matter of taste, so every skin shares
   it and only the drawing differs. *)
type trace_step = { label : string; locations : OutJ.location list }

let intermediate_label = "Taint flows through these intermediate variables:"

(* the locations of the intermediate variables, less the consecutive
   duplicates that would repeat a line *)
let locations_of_vars (vars : OutJ.match_intermediate_var list) :
    OutJ.location list =
  vars
  |> List.fold_left
       (fun (acc : OutJ.location list) (var : OutJ.match_intermediate_var) ->
         match acc with
         | (previous : OutJ.location) :: _
           when Int.equal previous.start.line var.location.start.line ->
             acc
         | _ -> var.location :: acc)
       []
  |> List.rev

let intermediate_step (vars : OutJ.match_intermediate_var list) :
    trace_step list =
  match locations_of_vars vars with
  | [] -> []
  | locations -> [ { label = intermediate_label; locations } ]

let rec steps_of_call_trace ~(reverse : bool) (label : string)
    (trace : OutJ.match_call_trace) : trace_step list =
  match trace with
  | OutJ.CliLoc (loc, _) -> [ { label; locations = [ loc ] } ]
  | OutJ.CliCall ((loc, _), vars, inner) ->
      if reverse then
        (* a source trace is read from the origin outwards *)
        steps_of_call_trace ~reverse label inner
        @ intermediate_step vars
        @ [ { label = "then call to:"; locations = [ loc ] } ]
      else
        ({ label; locations = [ loc ] } :: intermediate_step vars)
        @ steps_of_call_trace ~reverse "then reaches:" inner

let steps_of_dataflow_trace (trace : OutJ.match_dataflow_trace) :
    trace_step list =
  match (trace.taint_source, trace.taint_sink) with
  | Some source, Some sink ->
      steps_of_call_trace ~reverse:true "Taint comes from:" source
      @ (match trace.intermediate_vars with
        | Some vars -> intermediate_step vars
        | None -> [])
      @ steps_of_call_trace ~reverse:false
          "This is how taint reaches the sink:" sink
  | _ -> []

(* One located line (or several, when the location spans them), under
   [prefix] and in the skin's gutter, with the located span picked out.
   NOTE: We need to consider that the location can span > 1 lines, which
   seems to happen with matches related to macroexpanded clojure code. *)
let pp_trace_location ~(prefix : string) ~(gutter : int -> string)
    ~(gutter_blank : string) ~(highlight : Fmt.style list) ppf
    (loc : OutJ.location) : unit =
  let pp_highlight : string Fmt.t =
    List.fold_left (fun acc style -> Fmt.styled style acc) Fmt.string highlight
  in
  let start_line_num = loc.start.line in
  let end_line_num = loc.end_.line in
  let start_col = max 0 (loc.start.col - 1) in
  let end_col =
    if Int.(equal start_line_num end_line_num) then
      max start_col (loc.end_.col - 1)
    else max 0 (loc.end_.col - 1)
  in
  try
    let file_content = UFile.read_file loc.path in
    let lines = String.split_on_char '\n' file_content |> Array.of_list in
    (* the indentation the located lines share is dropped, as it is for the
       snippet of the match itself, so that a trace deep inside a function
       does not push the code off to the right *)
    let located, dedented =
      Array.sub lines (start_line_num - 1) (end_line_num - start_line_num + 1)
      |> Array.to_list |> dedent_lines
    in
    let start_col = max 0 (start_col - dedented) in
    let end_col = max start_col (end_col - dedented) in
    let lines_to_print =
      located |> String.concat ("\n" ^ prefix ^ gutter_blank)
    in
    let a, b, c = cut lines_to_print start_col end_col in
    Fmt.pf ppf "%s%s%s%a%s@." prefix (gutter start_line_num) a pp_highlight b c
  with
  | ex ->
      Log.debug (fun m ->
          m "Could not read file %a (line_num = %d, start_col = %d, end_col = %d): %s"
            Fpath.pp loc.path start_line_num start_col end_col
            (Exception.(catch ex |> to_string)));
      Log.debug (fun m -> m "Location: %a" OutJ.pp_location loc);
      ()

(* A trace drawn as a tree hanging off the match: one branch per step, the
   last one closing the list, and a spine joining them to the match above.

   [line_prefix] opens every line, and [glyph] renders the drawing
   characters, so a skin decides how they are styled and whether they carry
   anything of its own before them. *)
let pp_dataflow_tree ~(line_prefix : string) ~(glyph : string -> string)
    ~(gutter : int -> string) ~(gutter_blank : string)
    ~(highlight : Fmt.style list) ppf (trace : OutJ.match_dataflow_trace) :
    unit =
  let steps = steps_of_dataflow_trace trace in
  let last = List.length steps - 1 in
  steps
  |> List.iteri (fun (i : int) (step : trace_step) ->
         let is_last = Int.equal i last in
         (* the spine, which also joins the first step to the match *)
         Fmt.pf ppf "%s%s@." line_prefix (glyph "│");
         Fmt.pf ppf "%s%s%s@." line_prefix
           (glyph (if is_last then "└─ " else "├─ "))
           step.label;
         let prefix =
           line_prefix ^ glyph (if is_last then "   " else "│  ")
         in
         step.locations
         |> List.iter
              (pp_trace_location ~prefix ~gutter ~gutter_blank ~highlight ppf))

(* The taint trace of a match, as a flat sequence of steps.

   [indent] opens every line and [gutter] precedes the code of a located
   line, [gutter_blank] standing in for it on the continuation lines of a
   location that spans several; [gap] is what goes on the blank line before
   each step. A skin that colours any of them passes it already rendered,
   since whole lines are printed here rather than handed back. A skin that
   wants a different shape altogether builds it from
   steps_of_dataflow_trace and pp_trace_location. *)
let pp_dataflow_trace ?(indent = findings_indent)
    ?(gutter = fun (n : int) -> Printf.sprintf "%4d┆ " n)
    ?(gutter_blank = "    ┆ ") ?(gap = "")
    ?(highlight : Fmt.style list = [ `Bold ]) ppf
    (trace : OutJ.match_dataflow_trace) : unit =
  steps_of_dataflow_trace trace
  |> List.iter (fun (step : trace_step) ->
         Fmt.pf ppf "%s@.%s %s@." gap indent step.label;
         step.locations
         |> List.iter
              (pp_trace_location ~prefix:indent ~gutter ~gutter_blank
                 ~highlight ppf))
