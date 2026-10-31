(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The layout code shared by the skins of the text report.
 *
 * Wrapping, tab expansion and dedenting a snippet do not depend on the
 * skin, and neither does the content of a report: the lines of a match that
 * it shows and the part the match covers, how findings group by file, what
 * goes under a finding, and how a ci report splits its findings. How each
 * piece is drawn depends on the skin.
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

let styles (xs : Fmt.style list) (pp : 'a Fmt.t) : 'a Fmt.t =
  List.fold_left
    (fun (acc : 'a Fmt.t) (style : Fmt.style) -> Fmt.styled style acc)
    pp xs

(* an empty styled string would print only two escapes, so an empty span
   prints nothing *)
let pp_span (xs : Fmt.style list) (ppf : Format.formatter) (txt : string) :
    unit =
  if not (String.equal txt "") then styles xs Fmt.string ppf txt

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
   rich added to every line the wrapper printed.

   Called from a chunk with the log mutex held, so it must not log: see
   Skin.chunk. *)
let wrap_lines ~(filler : filler) ~(width : int) ~(initial_indent : int)
    ~(subsequent_indent : int) (txt : string) : (string * string) list =
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

(* A line of code prefixed with its number, wrapped, with the part
   [highlight_start, highlight_end) of the code styled with [highlight] and
   the rest of the code with [unhighlighted], across the wrapped chunks.

   The gutter is set by [number_indent] (the columns before the number),
   [number_width] (the width in which the number and its separator are
   right-aligned) and [code_indent] (the column where a wrapped chunk
   resumes, usually number_indent + number_width), and styled with
   [gutter_style]. The defaults are those of the legacy skin. *)
let pp_wrapped_code_line ?(number_indent = line_number_indent_size)
    ?(code_indent = code_indent_size) ?(number_width = line_number_width)
    ?(separator = "┆ ") ppf ~(line_number : int) ~(width : int)
    ~(gutter_style : Fmt.style list) ~(highlight : Fmt.style list)
    ~(unhighlighted : Fmt.style list) ~(highlight_start : int)
    ~(highlight_end : int) (line : string) : unit =
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
     included, so the highlighted range moves with the text *)
  let text, offset_of = munge_whitespace_with_offsets typed in
  let moved (i : int) : int =
    offset_of.(max 0 (min (String.length typed) (i + shift)))
  in
  let code_start = moved 0
  and highlight_start = moved highlight_start
  and highlight_end = moved highlight_end in
  let indentation =
    chunk_indentation ~initial_indent:number_indent
      ~subsequent_indent:code_indent
  in
  fill_chunks ~filler:Textwrap ~width ~initial_indent:number_indent
    ~subsequent_indent:code_indent text
  |> List.iteri (fun (i : int) ((offset : int), (length : int)) ->
         let chunk = String.sub text offset length in
         let within (pos : int) : int = max 0 (min length (pos - offset)) in
         (* the number and its separator are in the first chunk only *)
         let code_from = within code_start in
         let highlight_from = max code_from (within highlight_start) in
         let highlight_to = max highlight_from (within highlight_end) in
         let gutter = Str.first_chars chunk code_from in
         let before, highlighted, after =
           cut chunk highlight_from highlight_to
         in
         let before = Str.string_after before code_from in
         (* the highlight is styled even when it is empty, so that the
            legacy report keeps its escapes *)
         Fmt.pf ppf "%s%a%a%a%a@." (indentation i) (pp_span gutter_style)
           gutter (pp_span unhighlighted) before
           (styles highlight Fmt.string)
           highlighted (pp_span unhighlighted) after)

(* The lines of an autofix replacement, for a skin to prefix one by one,
   without leading and trailing blank lines; an empty result means the fix
   deletes the match.

   A fix replaces text in the middle of a line, so its first line has no
   indentation of its own while the lines below it have absolute
   indentation. Padding the first line to the start column of the match
   ([first_col], counted from 1 as in the output) restores the block, and
   dedenting it then keeps the indentation of the fix relative to itself. *)
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

(* What the location of a step of a trace is: the source, a call to a
   function in which the source is, an intermediate variable, a call that
   the taint goes through, the parameter by which it enters that function
   and the return by which it leaves it, a call to a function in which the
   sink is, or the sink. *)
type step_kind =
  | Source
  | Source_call
  | Intermediate
  | Intermediate_call
  | Parameter
  | Return
  | Sink_call
  | Sink

(* coupling: a kind added to [step_kind] must be added here too, since the
   labels of a trace are padded to the widest of these *)
let step_kinds : step_kind list =
  [
    Source;
    Source_call;
    Intermediate;
    Intermediate_call;
    Parameter;
    Return;
    Sink_call;
    Sink;
  ]

(* One step of a taint trace flattened into a sequence, as the legacy
   report prints it: its label and the locations it refers to. *)
type trace_step = {
  label : string;
  locations : OutJ.location list;
}

let intermediate_label = "Taint flows through these intermediate variables:"

let same_line (a : OutJ.location) (b : OutJ.location) : bool =
  Fpath.equal a.path b.path && Int.equal a.start.line b.start.line

(* the locations of the intermediate variables, without consecutive
   locations on the same line *)
let locations_of_vars (vars : OutJ.match_intermediate_var list) :
    OutJ.location list =
  vars
  |> List.fold_left
       (fun (acc : OutJ.location list) (var : OutJ.match_intermediate_var) ->
         match acc with
         | (previous : OutJ.location) :: _ when same_line previous var.location
           ->
             acc
         | _ -> var.location :: acc)
       []
  |> List.rev

let intermediate_step (vars : OutJ.match_intermediate_var list) :
    trace_step list =
  match locations_of_vars vars with
  | [] -> []
  | locations -> [ { label = intermediate_label; locations } ]

(* [reverse] holds for a source trace and not for a sink trace. *)
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
  let trace = Semgrep_output_utils.flat_dataflow_trace trace in
  match (trace.taint_source, trace.taint_sink) with
  | Some source, Some sink ->
      steps_of_call_trace ~reverse:true "Taint comes from:" source
      @ (match trace.intermediate_vars with
        | Some vars -> intermediate_step vars
        | None -> [])
      @ steps_of_call_trace ~reverse:false
          "This is how taint reaches the sink:" sink
  | _ -> []

(* A taint trace as a tree, as a skin draws it. A node is a step, with its
   location; a call to a function in which the source or the sink is, or
   that the taint goes through, has the steps inside that function under
   it. *)
type trace_node = {
  kind : step_kind;
  location : OutJ.location;
  children : trace_node list;
}

(* The steps of a run of intermediate variables: each variable is a step,
   of the kind of its role in the function it is in, without consecutive
   locations on the same line, and each call that the taint goes through is
   a step with the path inside the function under it. *)
let rec intermediate_nodes (vars : OutJ.match_intermediate_var list) :
    trace_node list =
  let plain (var : OutJ.match_intermediate_var) : bool =
    Option.is_none var.role && Option.is_none var.through_call
  in
  (* the steps in reverse *)
  vars
  |> List.fold_left
       (fun ((acc : trace_node list),
             (previous : OutJ.match_intermediate_var option))
            (var : OutJ.match_intermediate_var) ->
         match previous with
         | Some previous
           when plain previous && plain var
                && same_line previous.location var.location ->
             (acc, Some var)
         | _ ->
             let node =
               match (var.role, var.through_call) with
               | _, Some (call : OutJ.match_through_call) ->
                   {
                     kind = Intermediate_call;
                     location = var.location;
                     children = intermediate_nodes call.inside;
                   }
               | Some `Parameter, None ->
                   { kind = Parameter; location = var.location; children = [] }
               | Some `Return, None ->
                   { kind = Return; location = var.location; children = [] }
               | None, None ->
                   {
                     kind = Intermediate;
                     location = var.location;
                     children = [];
                   }
             in
             (node :: acc, Some var))
       ([], None)
  |> fst |> List.rev

(* [reverse] holds for a source trace and not for a sink trace. *)
let rec nodes_of_call_trace ~(reverse : bool) (trace : OutJ.match_call_trace)
    : trace_node list =
  match trace with
  | OutJ.CliLoc (loc, _) ->
      [
        {
          kind = (if reverse then Source else Sink);
          location = loc;
          children = [];
        };
      ]
  | OutJ.CliCall ((loc, _), vars, inner) ->
      let children =
        (* in the function called, the taint goes from the source to the
           return, or from the parameter to the sink *)
        if reverse then
          nodes_of_call_trace ~reverse inner @ intermediate_nodes vars
        else intermediate_nodes vars @ nodes_of_call_trace ~reverse inner
      in
      [
        {
          kind = (if reverse then Source_call else Sink_call);
          location = loc;
          children;
        };
      ]

let tree_of_dataflow_trace (trace : OutJ.match_dataflow_trace) :
    trace_node list =
  match (trace.taint_source, trace.taint_sink) with
  | Some source, Some sink ->
      nodes_of_call_trace ~reverse:true source
      @ intermediate_nodes (Option.value ~default:[] trace.intermediate_vars)
      @ nodes_of_call_trace ~reverse:false sink
  | _ -> []

(* False for a -e/--pattern run: Rule.rule_of_formula builds a rule with id
   "-", a default severity, and the pattern as its message. The legacy report
   prints no heading for such a finding (no severity, id or message), only
   the file name and the snippet, and every skin does the same. *)
let has_rule_name (m : OutJ.cli_match) : bool =
  not (Rule_ID.equal m.check_id Rule_ID.dash_e)

(* Under --interfile-dedup-by source-sink the engine emits one finding per
   source that reaches a sink, so several findings share a sink and differ
   only in their source; without grouping, a report would show the same
   block several times. *)
let same_sink (a : OutJ.cli_match) (b : OutJ.cli_match) : bool =
  Fpath.equal a.path b.path
  && Rule_ID.equal a.check_id b.check_id
  && Int.equal a.start.offset b.start.offset
  && Int.equal a.end_.offset b.end_.offset

(* Runs of adjacent findings that share a sink, in input order. Only
   neighbours are compared, so [matches] must be sorted by
   Semgrep_output_utils.sort_cli_matches, whose key (path, start and end
   positions, rule) makes the findings of one sink adjacent. On an unsorted
   list the groups are too small, and no error is raised. *)
let group_findings_by_sink (matches : OutJ.cli_match list) :
    OutJ.cli_match list list =
  List.fold_left
    (fun (groups : OutJ.cli_match list list) (m : OutJ.cli_match) ->
      match groups with
      | (previous :: _ as group) :: older when same_sink previous m ->
          (m :: group) :: older
      | _ -> [ m ] :: groups)
    [] matches
  |> List_.map List.rev |> List.rev

let one_line_of_code (code : string) : string =
  code |> String.split_on_char '\n' |> List_.map String.trim
  |> List.filter (fun (s : string) -> not (String.equal s ""))
  |> String.concat " "

(* the location of the taint source of a finding, and its code on one
   line *)
let source_of_finding (finding : OutJ.cli_match) :
    (OutJ.location * string) option =
  match finding.extra.dataflow_trace with
  | Some { OutJ.taint_source = Some source; _ } ->
      let loc, code = Core_json_output.leaf_of_call_trace source in
      Some (loc, one_line_of_code code)
  | Some _
  | None ->
      None

let sources_of_sink (findings : OutJ.cli_match list) :
    (OutJ.location * string) list =
  findings |> List_.filter_map source_of_finding

(* [s] cut to [width] characters, with an ellipsis for the dropped part. A
   source that spans several lines becomes one long line, which would
   otherwise exceed the width of the report.

   Counted in characters rather than bytes: a byte count would clip a CJK or
   accented line to a third of its width. A CJK character occupies two
   columns, so a line of them still overruns by a factor of two; the report
   does not model double-width characters. *)
let ellipsize ~(width : int) (s : string) : string =
  (* no room for any text: the ellipsis alone *)
  if width <= 1 then "…"
  else
    let offsets = Utf8.code_point_offsets s in
    (* the array carries the length of [s] as its last element, so it holds
       one more entry than the string has characters *)
    let characters = Array.length offsets - 1 in
    if characters <= width then s
    else String_.safe_sub s 0 offsets.(max 0 (width - 1)) ^ "…"

(* python compatibility: the 22m and 24m are "normal color or
    intensity", and "underline off" *)
let esc_prefix (ppf : Format.formatter) =
  if Fmt.style_renderer ppf = `Ansi_tty then Fmt.any "\027[22m\027[24m  "
  else Fmt.any "  "

let rec widest_line (acc : int) (nodes : trace_node list) : int =
  nodes
  |> List.fold_left
       (fun (acc : int) (node : trace_node) ->
         widest_line (max acc node.location.start.line) node.children)
       acc

(* The number of digits of the largest line number in the traces of these
   findings. A skin sizes its trace gutter from this rather than from the
   line of the finding, since a trace step can be in another file or much
   further down the same file. *)
let trace_line_digits (findings : OutJ.cli_match list) : int =
  let widest =
    findings
    |> List.fold_left
         (fun (acc : int) (finding : OutJ.cli_match) ->
           match finding.extra.dataflow_trace with
           | None -> acc
           | Some trace -> widest_line acc (tree_of_dataflow_trace trace))
         1
  in
  String.length (string_of_int (max 1 widest))

(*****************************************************************************)
(* What a report is made of *)
(*****************************************************************************)

(* A group of findings at its position in the report: one finding, or all
   those that share a sink under --interfile-dedup-by source-sink, [lead]
   being the first. [opens_file] is true for the first group of a file.
   [heading] is true when the group is printed with its rule and message,
   which is not the case when the previous group has the same file, rule
   and message (the message is compared too, since it carries the
   metavariables of its match). [continued] is true when the next group has
   no heading. *)
type placed = {
  lead : OutJ.cli_match;
  group : OutJ.cli_match list;
  opens_file : bool;
  heading : bool;
  continued : bool;
}

let place_findings (interfile_dedup_by : Core_match.interfile_dedup_by)
    (matches : OutJ.cli_match list) : placed list =
  let groups =
    match interfile_dedup_by with
    | Core_match.Sink -> List_.map (fun (m : OutJ.cli_match) -> [ m ]) matches
    | Core_match.Source_sink -> group_findings_by_sink matches
  in
  let headed =
    groups
    |> List.filter_map (fun (group : OutJ.cli_match list) ->
           match group with
           | lead :: _ -> Some (lead, group)
           | [] -> None)
    |> List.fold_left_map
         (fun (previous : OutJ.cli_match option)
              ((lead : OutJ.cli_match), (group : OutJ.cli_match list)) ->
           let opens_file, heading =
             match previous with
             | None -> (true, true)
             | Some (p : OutJ.cli_match) ->
                 let opens_file = not (Fpath.equal p.path lead.path) in
                 ( opens_file,
                   opens_file
                   || (not (Rule_ID.equal p.check_id lead.check_id))
                   || not (String.equal p.extra.message lead.extra.message) )
           in
           (Some lead, (lead, group, opens_file, heading)))
         None
    |> snd
  in
  let next_headings =
    match headed with
    | [] -> []
    | _ :: rest ->
        List_.map (fun (_, _, _, (heading : bool)) -> Some heading) rest
        @ [ None ]
  in
  List.combine headed next_headings
  |> List_.map (fun ((lead, group, opens_file, heading), next_heading) ->
         {
           lead;
           group;
           opens_file;
           heading;
           continued =
             (match next_heading with
             | Some (next : bool) -> not next
             | None -> false);
         })

(* A line of the code of a match, with the part of it the match covers
   as offsets into [text]. *)
type code_line = {
  line_number : int;
  text : string;
  match_start : int;
  match_end : int;
}

(* The code of a match as a report shows it: the lines after the autofix
   when there is one, without their common indentation, at most
   [max_lines_per_finding] of them (0 for all), and the number of lines left
   out. On a match within one line, the covered part has the length of the
   match. *)
let code_lines ~(max_lines_per_finding : int) (m : OutJ.cli_match) :
    code_line list * int option =
  let lines, dedented =
    Option.value
      ~default:(String.split_on_char '\n' m.extra.lines)
      m.extra.fixed_lines
    |> dedent_lines
  in
  let total = List.length lines in
  let keep =
    if max_lines_per_finding = 0 then total
    else min total max_lines_per_finding
  in
  let col (c : int) : int = max 0 (c - 1 - dedented) in
  let start_line = m.start.line in
  ( List_.take keep lines
    |> List.mapi (fun (i : int) (text : string) ->
           let line_number = start_line + i in
           let match_start =
             if line_number > start_line then 0 else col m.start.col
           in
           let match_end =
             max match_start
               (if line_number >= m.end_.line then
                  min
                    (if m.start.line = m.end_.line then
                       match_start + (m.end_.col - m.start.col)
                     else col m.end_.col)
                    (String.length text)
                else String.length text)
           in
           { line_number; text; match_start; match_end }),
    if keep = total then None else Some (total - keep) )

(* The definitions that enclose a match, with --output-enclosing-context,
   from the outermost to the innermost, each as its kind and its name. The
   engine records them innermost first. *)
let enclosing_context (m : OutJ.cli_match) : (string * string) list =
  m.extra.enclosing_context |> Option.value ~default:[]
  |> List.rev_map (fun (elem : OutJ.enclosing_context_elem) ->
         (elem.kind, elem.name))

(* What a report shows under the code of a finding: one entry for the
   finding, or for each finding of [group], which share its sink. For an
   interfile rule, the location and the code of the [source] of each, since
   for a flow across files the sink does not show where the untrusted value
   entered. With --dataflow-traces, the [trace] of each.

   A blank line precedes the block when it starts with a source, and
   separates two entries only when each has a trace: the vertical line of a
   trace already joins it to the snippet above, and a list of sources is
   printed without blank lines. *)
type origin = {
  finding : OutJ.cli_match;
  gap_before : bool;
  source : (OutJ.location * string) option;
  trace : OutJ.match_dataflow_trace option;
}

let origins ~(is_interfile : Rule_ID.t -> bool) ~(show_dataflow_traces : bool)
    (m : OutJ.cli_match) (group : OutJ.cli_match list) : origin list =
  let name_sources = is_interfile m.check_id in
  if not (name_sources || show_dataflow_traces) then []
  else
    (if List_.null group then [ m ] else group)
    |> List.mapi (fun (i : int) (finding : OutJ.cli_match) ->
           {
             finding;
             gap_before =
               (i = 0 && name_sources) || (i > 0 && show_dataflow_traces);
             source =
               (if name_sources then source_of_finding finding else None);
             trace =
               (if show_dataflow_traces then finding.extra.dataflow_trace
                else None);
           })

(* A line of the fix shown under a finding: the first, after the skin's
   label; a blank line, printed without indentation to avoid trailing
   whitespace; or a later line. *)
type fix_line =
  | Fix_first of string
  | Fix_blank
  | Fix_more of string

(* The fix of a finding as a report shows it: None without a fix, Some []
   for an empty fix, which deletes the match, and otherwise its lines, each
   wrapped at [width] as in the legacy report, since a one-line fix can be
   wider than the report. *)
let fix_display ~(width : int) (m : OutJ.cli_match) : fix_line list option =
  m.extra.fix
  |> Option.map (fun (fix : string) ->
         fix_lines ~first_col:m.start.col fix
         |> List.concat_map (fun (line : string) ->
                wrap_lines ~filler:Textwrap ~width ~initial_indent:0
                  ~subsequent_indent:0 line
                |> List_.map snd)
         |> List.mapi (fun (i : int) (txt : string) ->
                if i = 0 then Fix_first txt
                else if String.equal txt "" then Fix_blank
                else Fix_more txt))

let pp_time ppf (cli_output : OutJ.cli_output) : unit =
  (* the "time" field is there with --time *)
  match cli_output.time with
  | Some time ->
      (* python: a blank line separates the block from the findings, and
         the last finding printed one already *)
      if List_.null cli_output.results then Fmt.pf ppf "@.";
      Time_report.pp_time_summary ppf time cli_output.errors
  | None -> ()

(* The findings of a report, sorted, then the timing table. A ci report
   lists the blocking findings first, under their own heading, then the
   others under theirs, and ends with the rules of the blocking findings.
   The skin draws each piece: the findings by file, the heading of a ci
   section with its count, and the rules fired. *)
let pp_findings ~(is_ci_invocation : bool)
    ~(pp_by_file : OutJ.cli_match list Fmt.t)
    ~(pp_ci_heading : blocking:bool -> int Fmt.t)
    ~(pp_rules_fired : OutJ.cli_match list Fmt.t) ppf
    (cli_output : OutJ.cli_output) : unit =
  let sorted = cli_output.results |> Semgrep_output_utils.sort_cli_matches in
  (if not is_ci_invocation then pp_by_file ppf sorted
   else
     let blocking, advisory =
       List.partition
         (fun (m : OutJ.cli_match) -> is_blocking m.extra.metadata)
         sorted
     in
     let section ~(blocking : bool) (matches : OutJ.cli_match list) : unit =
       if not (List_.null matches) then begin
         pp_ci_heading ~blocking ppf (List.length matches);
         pp_by_file ppf matches
       end
     in
     section ~blocking:true blocking;
     section ~blocking:false advisory;
     pp_rules_fired ppf blocking);
  pp_time ppf cli_output

(* The path of a step outside the file of the finding, so that a trace
   across files can be followed. *)
let step_path ?(finding_path : Fpath.t option) (loc : OutJ.location) :
    Fpath.t option =
  match finding_path with
  | Some path when not (Fpath.equal loc.path path) -> Some loc.path
  | Some _
  | None ->
      None

let pp_step_path ~(prefix : string) ppf (path : Fpath.t) : unit =
  Fmt.pf ppf "%s%a@." prefix
    Fmt.(styled (`Fg `Cyan) (esc_prefix ppf ++ string))
    (Fpath.to_string path)

let located_lines (loc : OutJ.location) : string list =
  let lines =
    UFile.read_file loc.path |> String.split_on_char '\n' |> Array.of_list
  in
  Array.sub lines (loc.start.line - 1) (loc.end_.line - loc.start.line + 1)
  |> Array.to_list

(* A location as the legacy report prints it: the located lines joined into
   one string, with the highlight cut out of it at the columns of the
   location. For a location that spans lines, these are the start column of
   the first line and the end column of the last, so the highlight ends
   wherever the end column falls in the joined text, and a location whose
   end column precedes its start column raises in [cut] and is not
   printed.

   NOTE: We need to consider that the location can span > 1 lines, which
   seems to happen with matches related to macroexpanded clojure code.

   python: the output of pysemgrep, which the legacy skin keeps *)
let pp_joined_location ?(finding_path : Fpath.t option) ~(prefix : string)
    ~(gutter : int -> string) ~(gutter_blank : string)
    ~(highlight : Fmt.style list) ppf (loc : OutJ.location) : unit =
  let pp_highlight : string Fmt.t = styles highlight Fmt.string in
  let start_line_num = loc.start.line in
  let start_col = max 0 (loc.start.col - 1) in
  let end_col =
    if Int.(equal start_line_num loc.end_.line) then
      max start_col (loc.end_.col - 1)
    else max 0 (loc.end_.col - 1)
  in
  try
    let lines_to_print =
      located_lines loc |> String.concat ("\n" ^ prefix ^ gutter_blank)
      (* Below is an example of what can be printed now, while it would raise an
       * exception previously, showing nothing in the output. In clojure, this is
       * macroexpanded to:
       *   (sink (:user x))
       * before matching and the resulting range is applied to the original term:
       *  
       * This is how taint reaches the sink:
       *   338┆   (some-> x
       *      ┆       (:user)
       *      ┆       (sink)))
       *)
    in
    let a, b, c = cut lines_to_print start_col end_col in
    step_path ?finding_path loc |> Option.iter (pp_step_path ~prefix ppf);
    Fmt.pf ppf "%s%s%s%a%s@." prefix (gutter start_line_num) a pp_highlight b c
  with
  | ex ->
      Log.debug (fun m ->
          m "Could not read file %a (line_num = %d, start_col = %d, end_col = %d): %s"
            Fpath.pp loc.path start_line_num start_col end_col
            (Exception.(catch ex |> to_string)));
      Log.debug (fun m -> m "Location: %a" OutJ.pp_location loc);
      ()

(* The part of each located line that the location covers: from the start
   column on the first line, and up to the end column on the last. A line
   after the first is covered from its first non-blank character, so that a
   highlight with a background does not paint the indentation. Columns are
   0-based offsets into the lines as given. *)
let covered_spans ~(start_col : int) ~(end_col : int) (lines : string list) :
    (int * int) list =
  let last = List.length lines - 1 in
  lines
  |> List.mapi (fun (i : int) (line : string) ->
         let len = String.length line in
         let rec first_non_blank (k : int) : int =
           if k < len && (line.[k] = ' ' || line.[k] = '\t') then
             first_non_blank (k + 1)
           else k
         in
         let from = if i = 0 then min start_col len else first_non_blank 0 in
         let upto = if i = last then min end_col len else len in
         (from, max from upto))

(* The located lines, after [prefix] and the skin's gutter, with the covered
   span highlighted on each line. Each line is printed separately, with
   [gutter_blank] in place of the gutter after the first, so that the
   highlight never extends into the prefix or the gutter, which a skin may
   style.

   [finding_path] is the file of the finding: the path of a step in another
   file is printed, so that a trace across files can be followed.

   [first_prefix] takes the place of [prefix] on the first line printed,
   the path or the first located line, where a skin can put a label.

   [dedent] removes the common indentation of the located lines, so that a
   step deep inside a function is not shifted to the right. *)
let pp_trace_location ?(finding_path : Fpath.t option) ?(dedent = false)
    ~(first_prefix : string) ~(prefix : string) ~(gutter : int -> string)
    ~(gutter_blank : string) ~(highlight : Fmt.style list)
    ~(unhighlighted : Fmt.style list) ppf (loc : OutJ.location) : unit =
  let path = step_path ?finding_path loc in
  path |> Option.iter (pp_step_path ~prefix:first_prefix ppf);
  let lead (i : int) : string =
    if Int.equal i 0 && Option.is_none path then first_prefix else prefix
  in
  match located_lines loc with
  (* a file that cannot be read, or lines that it does not have *)
  | exception ((Sys_error _ | Unix.Unix_error _ | Invalid_argument _) as ex)
    ->
      Log.debug (fun m ->
          m "Could not read file %a (lines %d-%d): %s" Fpath.pp loc.path
            loc.start.line loc.end_.line
            (Exception.(catch ex |> to_string)));
      Log.debug (fun m -> m "Location: %a" OutJ.pp_location loc);
      (* the line number and an ellipsis for the code, so that a step of a
         trace keeps its label *)
      Fmt.pf ppf "%s%s%a@." (lead 0) (gutter loc.start.line)
        (pp_span unhighlighted) "…"
  | located ->
      let located, dedented =
        if dedent then dedent_lines located else (located, 0)
      in
      let spans =
        covered_spans
          ~start_col:(max 0 (loc.start.col - 1 - dedented))
          ~end_col:(max 0 (loc.end_.col - 1 - dedented))
          located
      in
      List.combine located spans
      |> List.iteri
           (fun (i : int) ((line : string), ((from : int), (upto : int))) ->
             let gutter =
               if Int.equal i 0 then gutter loc.start.line else gutter_blank
             in
             let before = String.sub line 0 from
             and covered = String.sub line from (upto - from)
             and after = Str.string_after line upto in
             Fmt.pf ppf "%s%s%a%a%a@." (lead i) gutter (pp_span unhighlighted)
               before (pp_span highlight) covered (pp_span unhighlighted) after)

(* A trace drawn as a tree under the match: one branch per node, the last
   one closing its level, and a vertical line joining the first branch to
   the match above. The first line of a node starts with the [label] of its
   kind, padded to the widest label of all kinds; the nodes under a call are
   one level deeper, under the label of the call.

   [line_prefix] starts every line, and [glyph] renders the box-drawing
   characters, so that a skin sets their style and any text before them. *)
let pp_dataflow_tree ?(finding_path : Fpath.t option) ?(dedent = true)
    ~(line_prefix : string) ~(glyph : string -> string)
    ~(gutter : int -> string) ~(gutter_blank : string)
    ~(highlight : Fmt.style list) ~(unhighlighted : Fmt.style list)
    ~(label : step_kind -> string) ppf (trace : OutJ.match_dataflow_trace) :
    unit =
  let width =
    step_kinds
    |> List.fold_left
         (fun (acc : int) (kind : step_kind) ->
           max acc (Utf8.length (label kind)))
         0
  in
  (* [indent] holds the stems of the levels above, before the branches of
     [nodes] *)
  let rec pp_nodes (indent : string) (nodes : trace_node list) : unit =
    let last = List.length nodes - 1 in
    nodes
    |> List.iteri (fun (i : int) (node : trace_node) ->
           let is_last = Int.equal i last in
           let branch = glyph (if is_last then "└─ " else "├─ ")
           and stem = indent ^ glyph (if is_last then "   " else "│  ") in
           let txt = label node.kind in
           let first_prefix =
             line_prefix ^ indent ^ branch ^ txt
             ^ String.make (width - Utf8.length txt + 1) ' '
           (* the lines after the first of a node with nodes under it carry
              the vertical line that joins them to the node *)
           and prefix =
             line_prefix ^ stem
             ^
             if List_.null node.children then String.make (width + 1) ' '
             else glyph "│" ^ String.make width ' '
           in
           pp_trace_location ?finding_path ~dedent ~first_prefix ~prefix
             ~gutter ~gutter_blank ~highlight ~unhighlighted ppf node.location;
           pp_nodes stem node.children)
  in
  let nodes = tree_of_dataflow_trace trace in
  (* the vertical line that joins the first step to the match *)
  if not (List_.null nodes) then Fmt.pf ppf "%s%s@." line_prefix (glyph "│");
  pp_nodes "" nodes

(* The taint trace of a match as a flat sequence of steps, as the legacy
   report prints it. [indent] starts every line and [gutter] precedes the
   code of a located line, with [gutter_blank] in its place on the
   continuation lines of a location that spans several; [gap] is printed on
   the blank line before each step.

   The locations are printed by pp_joined_location, which cuts the highlight
   out of the located lines joined with [gutter_blank]; a skin that styles
   its gutter uses pp_dataflow_tree, or steps_of_dataflow_trace and
   pp_trace_location, instead. *)
let pp_dataflow_trace ?(finding_path : Fpath.t option)
    ?(indent = findings_indent)
    ?(gutter = fun (n : int) -> Printf.sprintf "%4d┆ " n)
    ?(gutter_blank = "    ┆ ") ?(gap = "")
    ?(highlight : Fmt.style list = [ `Bold ]) ppf
    (trace : OutJ.match_dataflow_trace) : unit =
  steps_of_dataflow_trace trace
  |> List.iter (fun (step : trace_step) ->
         Fmt.pf ppf "%s@.%s %s@." gap indent step.label;
         step.locations
         |> List.iter
              (pp_joined_location ?finding_path ~prefix:indent ~gutter
                 ~gutter_blank ~highlight ppf))
