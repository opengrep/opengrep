module OutJ = Semgrep_output_v1_t
open Fpath_.Operators
module Log = Log_reporting.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  Partially translated from formatters/text.py
*)

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

type report_group =
  [ OutJ.validation_state
  | `Unreachable
  | `Undetermined
  | `Reachable
  | `Nonblocking
  | `Blocking
  | `Merged ]

let group_titles : report_group -> string = function
  | `Unreachable -> "Unreachable Supply Chain Finding"
  | `Undetermined -> "Undetermined Supply Chain Finding"
  | `Reachable -> "Reachable Supply Chain Finding"
  | `Nonblocking -> "Non-blocking Code Finding"
  | `Blocking -> "Blocking Code Finding"
  | `Merged -> "Code Finding"
  | `Confirmed_valid -> "Valid Secrets Finding"
  | `Confirmed_invalid -> "Invalid Secrets Finding"
  | `Validation_error -> "Secrets Validation Error"
  | `No_validator -> "Unvalidated Secrets Finding"

let sort_by_groups als =
  (* This is the order that groups will be desplayed in. *)
  let group_order : report_group -> int = function
    | `Blocking -> 1
    | `Reachable -> 2
    | `Confirmed_valid -> 3
    | `Undetermined -> 4
    | `Validation_error -> 5
    | `No_validator -> 6
    | `Nonblocking -> 7
    | `Unreachable -> 8
    | `Confirmed_invalid -> 9
    | `Merged -> 10
  in
  let compare_group x y = group_order x - group_order y in
  als |> List.stable_sort (Common.on compare_group fst)

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
   at the last space that fits, at the last hyphen between two letters, or
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
  let chunks : (int * int) list =
    let rec spaces (i : int) : int = if is_space i then spaces (i + 1) else i in
    let rec word (i : int) : int =
      if i >= n || is_space i then i
      else if ends_chunk i then i + 1
      else word (i + 1)
    in
    let rec go (i : int) (acc : (int * int) list) : (int * int) list =
      if i >= n then List.rev acc
      else
        let j = if is_space i then spaces i else word i in
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
   [bold_start, bold_end) of the code carried across the wrapped chunks. *)
let pp_wrapped_code_line ppf ~(line_number : int) ~(width : int)
    ~(bold_start : int) ~(bold_end : int) (line : string) : unit =
  let prefix =
    (* python: f"{line_number}┆ ".rjust(5); the separator is one column of
       three bytes *)
    let text = string_of_int line_number ^ "┆ " in
    let columns = String.length text - 2 in
    String.make (max 0 (line_number_width - columns)) ' ' ^ text
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
    chunk_indentation ~initial_indent:line_number_indent_size
      ~subsequent_indent:code_indent_size
  in
  fill_chunks ~filler:Textwrap ~width ~initial_indent:line_number_indent_size
    ~subsequent_indent:code_indent_size text
  |> List.iteri (fun (i : int) ((offset : int), (length : int)) ->
         let chunk = String.sub text offset length in
         let bold_from = max 0 (min length (bold_start - offset)) in
         let bold_to = max bold_from (min length (bold_end - offset)) in
         let a, b, c = cut chunk bold_from bold_to in
         Fmt.pf ppf "%s%s%a%s@." (indentation i) a
           Fmt.(styled `Bold string)
           b c)

let pp_dataflow_trace ppf (trace : OutJ.match_dataflow_trace) =
  (* Helper to print a location with bold highlighting *)
  (* NOTE: We need to consider that the location can span > 1 lines, which
   * seems to happen with matches related to macroexpanded clojure code. *)
  let print_location prefix (loc : OutJ.location) =
    let start_line_num = loc.start.line in
    let end_line_num = loc.end_.line in
    let start_col = max 0 (loc.start.col - 1) in
    let end_col =
      if Int.(equal start_line_num end_line_num)
      then max start_col (loc.end_.col - 1) else (max 0 (loc.end_.col - 1))
    in
    try
      let file_content = UFile.read_file loc.path in
      let lines = String.split_on_char '\n' file_content |> Array.of_list in
      let lines_to_print =
        Array.sub lines (start_line_num - 1) (end_line_num - start_line_num + 1)
        |> Array.to_list |> String.concat ("\n" ^ prefix ^ "    ┆ ")
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
      Fmt.pf ppf "%s%4d┆ %s%a%s@." prefix start_line_num a
        Fmt.(styled `Bold string) b c
    with ex ->
      Log.debug (fun m ->
          m "Could not read file %a (line_num = %d, start_col = %d, end_col = %d): %s"
            Fpath.pp loc.path
            start_line_num
            start_col
            end_col
            (Exception.(catch ex |> to_string)));
      Log.debug (fun m ->
          m "Location: %a"
            OutJ.pp_location loc);
      ()
  in

  (* Helper to print tokens with no consecutive duplicates *)
  let print_tokens_no_consec_dupes prefix tokens =
    let last_line = ref None in
    List.iter (fun (var : OutJ.match_intermediate_var) ->
      let loc = var.location in
      if Some loc.start.line <> !last_line then (
        last_line := Some loc.start.line;
        print_location prefix loc)
      ) tokens
  in

  (* Recursive function to print call trace *)
  let rec print_call_trace ?(reverse = false) label (trace : OutJ.match_call_trace) =
    match trace with
    | OutJ.CliLoc (loc, _) ->
        Fmt.pf ppf "@.%s %s@." findings_indent label;
        print_location findings_indent loc
    | OutJ.CliCall ((loc, _), intermediate_vars, inner_trace) ->
        if reverse (* this is for source taint traces *)
        then
        (print_call_trace ~reverse label inner_trace;
         if List.length intermediate_vars > 0 then (
           Fmt.pf ppf "@.%s Taint flows through these intermediate variables:@." findings_indent;
           print_tokens_no_consec_dupes findings_indent intermediate_vars);
         Fmt.pf ppf "@.%s %s@." findings_indent "then call to:" ;
         print_location findings_indent loc)
        else
        (Fmt.pf ppf "@.%s %s@." findings_indent label;
         print_location findings_indent loc;
         if List.length intermediate_vars > 0 then (
           Fmt.pf ppf "@.%s Taint flows through these intermediate variables:@." findings_indent;
           print_tokens_no_consec_dupes findings_indent intermediate_vars);
         print_call_trace ~reverse "then reaches:" inner_trace)
  in

  match (trace.taint_source, trace.taint_sink) with
  | Some source, Some sink ->
      print_call_trace ~reverse:true "Taint comes from:" source;
      (match trace.intermediate_vars with
      | Some vars when List.length vars > 0 ->
          Fmt.pf ppf "@.%s Taint flows through these intermediate variables:@." findings_indent;
          print_tokens_no_consec_dupes findings_indent vars
      | _ -> ());
      print_call_trace "This is how taint reaches the sink:" sink
  | _ -> ()

let pp_finding ~max_chars_per_line ~max_lines_per_finding ~color_output
    ~show_dataflow_traces ~append_separator ppf (m : OutJ.cli_match) =
  (* TODO: honour color_output, so that colours are decided per destination
   * as in the python wrapper, where a text file gets them under
   * SEMGREP_FORCE_COLOR. They currently come from the style renderer that
   * Logs_ sets on the formatter, from --force-color or a tty, which this
   * argument cannot override. *)
  ignore color_output;
  let lines =
    Option.value
      ~default:(String.split_on_char '\n' m.extra.lines)
      m.extra.fixed_lines
  in
  let lines, dedented = dedent_lines lines in
  let lines, trimmed =
    let ll = List.length lines in
    let max_lines =
      if max_lines_per_finding = 0 then ll else max_lines_per_finding
    in
    let keep = min ll max_lines in
    if keep = ll then (lines, None)
    else (List_.take keep lines, Some (ll - keep))
  in
  let start_line = m.start.line in
  (* python: per_line_max_chars_limit, the whole rendered line being
     wrapped at --max-chars-per-line, or at the width of the findings
     block when the flag asks for more *)
  let width =
    safe_width
      (if max_chars_per_line > 0 then min max_chars_per_line findings_text_width
       else findings_text_width)
  in
  lines
  |> List.iteri (fun (i : int) (line : string) ->
         let line_number = start_line + i in
         let col c = max 0 (c - 1 - dedented) in
         let bold_start = if line_number > start_line then 0 else col m.start.col in
         let bold_end =
           max bold_start
             (if line_number >= m.end_.line then
                min
                  (if m.start.line = m.end_.line then
                     bold_start + (m.end_.col - m.start.col)
                   else col m.end_.col)
                  (String.length line)
              else String.length line)
         in
         (* TODO(secrets): Apply masking to the bold part *)
         pp_wrapped_code_line ppf ~line_number ~width ~bold_start ~bold_end
           line);
  (match m.extra.dataflow_trace with
  | Some trace -> if show_dataflow_traces then pp_dataflow_trace ppf trace else ()
  | None -> ());
  match trimmed with
  | Some num ->
      Fmt.pf ppf
        "%s [hid %d additional lines, adjust with --max-lines-per-finding]@."
        findings_indent num
  | None ->
      if append_separator then
        Fmt.pf ppf "%s⋮┆%s" findings_indent (String.make fill_count '-')

(* TODO: factorize more this code, just the color and >>> change below *)
let pp_styled_severity ppf (severity : OutJ.match_severity) =
  match severity with
  | `Critical ->
      Fmt.pf ppf "%s%a" rule_leading_indent
        Fmt.(styled (`Fg `Magenta) string)
        "❯❯❯❱"
  | `Error
  | `High ->
      Fmt.pf ppf "%s%a" rule_leading_indent Fmt.(styled (`Fg `Red) string) "❯❯❱"
  | `Warning
  | `Medium ->
      Fmt.pf ppf "%s%a" rule_leading_indent
        Fmt.(styled (`Fg `Yellow) string)
        " ❯❱"
  | `Info
  | `Low ->
      Fmt.pf ppf "%s%a" rule_leading_indent
        Fmt.(styled (`Fg `Green) string)
        "  ❱"
  | `Inventory
  | `Experiment ->
      Fmt.pf ppf "%s%s" rule_leading_indent "   "

let pp_text_outputs ~max_chars_per_line ~max_lines_per_finding
    ~color_output ~show_dataflow_traces ppf
    (matches : OutJ.cli_match list) =
  let print_one_match ~(prev : OutJ.cli_match option) ~(cur : OutJ.cli_match)
      ~(next : OutJ.cli_match option) =
    (* Separation of concerns:
       Keep side effect separate from value-returning computations *)
    (match prev with
    | None -> Fmt.pf ppf "@."
    | Some _ -> ());
    (* Nesting hierarchy:
       file > rule > message derived from template in rule *)
    let must_print_file =
      (* must print file because it's a match in a new file *)
      match prev with
      | None -> true
      | Some m -> m.path <> cur.path
    in
    (* the rule name and its message are printed together, for a match of
       a new rule or with a different message (the message is derived from
       a template of the rule) *)
    let must_print_rule =
      must_print_file
      ||
      match prev with
      | None -> true
      | Some m ->
          (not (Rule_ID.equal m.check_id cur.check_id))
          || not (String.equal m.extra.message cur.extra.message)
    in
    let has_rule_name = cur.check_id <> Rule_ID.dash_e in
    (if must_print_file then
       (* python compatibility: the 22m and 24m are "normal color or
           intensity", and "underline off" *)
       let esc =
         if Fmt.style_renderer ppf = `Ansi_tty then Fmt.any "\027[22m\027[24m  "
         else Fmt.any "  "
       in
       Fmt.pf ppf "  %a@." Fmt.(styled (`Fg `Cyan) (esc ++ string)) !!(cur.path));
    (if must_print_rule then
       let rule_name_lines =
         if has_rule_name then (
           pp_styled_severity ppf cur.extra.severity;
           (* python: RULE_TEXT_WIDTH and RULE_INDENT *)
           wrap_lines ~filler:Textwrap ~width:(safe_width rule_text_width)
             ~initial_indent:0
             ~subsequent_indent:(rule_indent_size - console_indent_size)
             (Rule_ID.to_string cur.check_id))
         else []
       in
       match rule_name_lines with
       | [] -> ()
       | (_, txt) :: rest ->
           (* Print indented severity with 1 trailing space and then
              first line *)
           Fmt.pf ppf " %a@." Fmt.(styled `Bold string) txt;
           List.iter
             (fun (indentation, txt) ->
               Fmt.pf ppf "%s%a@." indentation Fmt.(styled `Bold string) txt)
             rest;
           (* python: DESC_TEXT_WIDTH and BASE_INDENT, the message being
              filled paragraph by paragraph *)
           cur.extra.message |> message_paragraphs
           |> List.iteri
                (fun (i : int) ((extra_indent : int), (paragraph : string)) ->
                  if i > 0 then Fmt.pf ppf "@.";
                  let indent =
                    detail_indent_size - console_indent_size + extra_indent
                  in
                  wrap_lines ~filler:Click ~width:(safe_width desc_text_width)
                    ~initial_indent:indent ~subsequent_indent:indent paragraph
                  |> List.iter (fun (indentation, txt) ->
                         Fmt.pf ppf "%s%s@." indentation txt));
           (match metadata_member "shortlink" cur.extra.metadata with
           | `String txt -> Fmt.pf ppf "%sDetails: %s@." detail_indent txt
           | _ -> ());
           Fmt.pf ppf "@.");
    (match cur.extra.fix with
    | None -> ()
    | Some fix ->
        (* the fix on one line, wrapped after the tag; an empty fix deletes
           the match *)
        let autofix_tag = "▶▶┆ Autofix ▶ " in
        let fix_text =
          fix |> String.split_on_char '\n' |> List_.map String.trim
          |> List.filter (fun (s : string) -> not (String.equal s ""))
          |> String.concat " "
        in
        (* python: (BASE_INDENT + 1) columns, plus those of the console *)
        Fmt.pf ppf "%s%a"
          (String.make (detail_indent_size + 1) ' ')
          Fmt.(styled (`Fg `Green) string)
          autofix_tag;
        if String.equal fix_text "" then
          Fmt.pf ppf "%a@." Fmt.(styled (`Fg `Red) string) "delete"
        else
          (* python: AUTOFIX_TEXT_WIDTH, and BASE_INDENT + 4 for the line
             number of the wrapped lines *)
          wrap_lines ~filler:Textwrap ~width:(safe_width autofix_text_width)
            ~initial_indent:0
            ~subsequent_indent:(detail_indent_size + 4 - console_indent_size)
            fix_text
          |> List.iteri (fun (i : int) ((indentation : string), (txt : string)) ->
                 if i = 0 then Fmt.pf ppf "%s@." txt
                 else Fmt.pf ppf "%s%s@." indentation txt));
    let same_file_next =
      match next with
      | None -> false
      | Some next -> Fpath.equal next.path cur.path
    in
    let same_rule_next =
      match next with
      | None -> false
      | Some next -> Rule_ID.equal next.check_id cur.check_id
    in
    pp_finding ~max_chars_per_line ~max_lines_per_finding ~color_output
      ~show_dataflow_traces ~append_separator:(same_file_next && same_rule_next)
      ppf cur;
    Fmt.pf ppf "@."
  in
  List_.iter_with_view_into_neighbor_elements print_one_match matches

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* the check_ids of the blocking rules behind [matches], for the
 * "RULES FIRED" sections of the ci output; "-" is the id of a -e/--pattern
 * rule and is discarded like in pyopengrep text.py *)
let blocking_rule_ids (matches : OutJ.cli_match list) : string list =
  matches
  |> List_.map (fun (m : OutJ.cli_match) -> Rule_ID.to_string m.check_id)
  |> List.filter (fun id -> not (String.equal id "-"))
  |> Set_.of_list |> Set_.elements |> List.sort String.compare

let pp_rules_fired ppf (title : string) (ids : string list) : unit =
  if not (List_.null ids) then (
    Fmt.pf ppf "@.  %s@." title;
    ids |> List.iter (fun id -> Fmt.pf ppf "    %s@." id))

let pp_cli_output
    ~max_chars_per_line
    ~max_lines_per_finding
    ~color_output
    ~show_dataflow_traces
    ?(is_ci_invocation = false)
    ppf
    (cli_output : OutJ.cli_output) =
  let groups =
    cli_output.results |> Semgrep_output_utils.sort_cli_matches
    |> Assoc.group_by (fun (m : OutJ.cli_match) ->
           match Product.of_cli_match m with
           | `SCA ->
               (* TO PORT:
                         subgroup = match.exposure_type or "undetermined"

                          figuring out the product, python uses (rule.py):
                             RuleProduct.sca
                             if "r2c-internal-project-depends-on" in self._raw
                             else RuleProduct.sast

                          and exposure_type (rule_match.py):
                          if "sca_info" not in self.extra:
                              return None

                          if self.metadata.get("sca-kind") == "upgrade-only":
                              return "reachable"
                          elif self.metadata.get("sca-kind") == "legacy":
                              return "undetermined"
                          else:
                              return "reachable" if self.extra["sca_info"].reachable else "unreachable" *)
               `Undetermined
           | `SAST when is_blocking m.extra.metadata -> `Blocking
           | `SAST -> `Nonblocking
           | `Secrets ->
               (Option.value ~default:`No_validator m.extra.validation_state
                 :> report_group))
  in
  let groups =
    (* from text.py:
       if not is_ci_invocation: *)
    if is_ci_invocation then groups
    else
      let merged =
        (try List.assoc `Nonblocking groups with
        | Not_found -> [])
        @
        try List.assoc `Blocking groups with
        | Not_found -> []
      in
      (`Merged, merged)
      :: List.filter
           (fun (k, _) -> not (k = `Nonblocking || k = `Blocking))
           groups
  in
  groups |> sort_by_groups
  |> List.iter (fun (group, matches) ->
         if not (List_.null matches) then
           Fmt_.pp_heading ppf
             (String_.unit_str (List.length matches) (group_titles group));
         pp_text_outputs ~max_chars_per_line ~max_lines_per_finding
           ~color_output ~show_dataflow_traces ppf matches);
  if is_ci_invocation then (
    pp_rules_fired ppf "BLOCKING CODE RULES FIRED:"
      (match List.assoc_opt `Blocking groups with
      | Some matches -> blocking_rule_ids matches
      | None -> []);
    (* fork: without secrets validators every Secrets match lands in the
     * No_validator group, where pyopengrep falls back to the plain
     * "dev.semgrep.actions" check *)
    let secrets_ids =
      ([ `Confirmed_valid; `Confirmed_invalid; `Validation_error; `No_validator ]
        : report_group list)
      |> List.concat_map (fun g ->
             match List.assoc_opt g groups with
             | Some matches ->
                 matches
                 |> List.filter (fun (m : OutJ.cli_match) ->
                        is_blocking m.extra.metadata)
                 |> blocking_rule_ids
             | None -> [])
      |> Set_.of_list |> Set_.elements |> List.sort String.compare
    in
    pp_rules_fired ppf "BLOCKING SECRETS RULES FIRED:" secrets_ids);
  (* the "time" field is there with --time *)
  match cli_output.time with
  | Some time ->
      (* python: a blank line separates the block from the findings, and
         the last finding printed one already *)
      if List_.null cli_output.results then Fmt.pf ppf "@.";
      Time_report.pp_time_summary ppf time cli_output.errors
  | None -> ()
