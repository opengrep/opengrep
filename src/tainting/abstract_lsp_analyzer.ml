(* Abstract LSP-based dependency analyzer using OCamlGraph *)

open AST_generic

module G =
  Graph.Imperative.Digraph.ConcreteLabeled
    (struct
      type t = Tok.location (* (function_name, file_path) *)

      let compare = Tok.compare_location
      let hash = Hashtbl.hash
      let equal = Tok.equal_location
    end)
    (struct
      type t = string

      let compare = String.compare
      let default = ""
    end)

module Display = struct
  include G

  let vertex_name Tok.{ str; pos } =
    "\"" ^ str ^ " at l: " ^ string_of_int pos.line ^ " c: "
    ^ string_of_int pos.column ^ "\n file " ^ Fpath.to_string pos.file^"\""

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Dot = Graph.Graphviz.Dot (Display)

type language_config = {
  lsp_command : string;
  lsp_args : string array;
  file_extension : string;
  language_id : string;
  parse_file : string -> (entity * definition_kind) list * stmt list;
}

type function_node = {
  name : string;
  file : string;
  ast_entity : entity;
  ast_definition : definition_kind;
  lsp_location : string * int * int; (* uri, line, char *)
}

type call_site = {
  caller : function_node;
  callee : function_node;
  call_expr : expr;
  location : string * int * int;
}

type dependency_analysis = {
  functions : function_node list;
  call_sites : call_site list;
  graph : G.t;
}

let empty_analysis () = { functions = []; call_sites = []; graph = G.create () }

(* Language configurations *)
let python_config =
  {
    lsp_command = "basedpyright-langserver";
    lsp_args = [| "basedpyright-langserver"; "--stdio" |];
    file_extension = ".py";
    language_id = "python";
    parse_file =
      (fun filename ->
        let python_ast = Parse_python.parse_program (Fpath.v filename) in
        let generic_ast = Python_to_generic.program python_ast in
        let functions = ref [] in
        List.iter
          (fun item ->
            match item with
            | { s = DefStmt (entity, definition); _ } -> (
                match entity with
                | { name = EN (Id ((_func_name, _tok), _)); _ } ->
                    functions := (entity, definition) :: !functions
                | _ -> ())
            | _ -> ())
          generic_ast;
        (!functions, generic_ast));
  }

let ocaml_config =
  {
    lsp_command = "ocamllsp";
    lsp_args = [| "ocamllsp" |];
    file_extension = ".ml";
    language_id = "ocaml";
    parse_file =
      (fun _filename ->
        (* For now, return empty - would need OCaml parser integration *)
        ([], []));
  }

let go_config =
  {
    lsp_command = "gopls";
    lsp_args = [| "gopls"; "serve" |];
    file_extension = ".go";
    language_id = "go";
    parse_file =
      (fun _filename ->
        (* For now, return empty - would need Go parser integration *)
        ([], []));
  }

(* LSP communication helpers *)
let send_lsp_message out_channel message =
  let content = Yojson.Safe.to_string message in
  let header =
    Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length content)
  in
  output_string out_channel header;
  output_string out_channel content;
  flush out_channel

let read_lsp_message in_channel =
  let content_length = ref 0 in
  let rec read_headers () =
    let line = input_line in_channel in
    (if String.starts_with ~prefix:"Content-Length:" line then
       let parts = String.split_on_char ':' line in
       match parts with
       | [ _; length_str ] ->
           content_length := int_of_string (String.trim length_str)
       | _ -> ());
    if String.trim line = "" then () else read_headers ()
  in
  read_headers ();
  let buffer = Bytes.create !content_length in
  really_input in_channel buffer 0 !content_length;
  Bytes.to_string buffer

let rec read_until_response in_channel target_id =
  let message = read_lsp_message in_channel in
  try
    let json = Yojson.Safe.from_string message in
    let id_opt =
      try Some (Yojson.Safe.Util.member "id" json) with
      | _ -> None
    in
    match id_opt with
    | Some id when id = `Int target_id -> message
    | Some _ -> read_until_response in_channel target_id
    | None -> read_until_response in_channel target_id
  with
  | _e -> read_until_response in_channel target_id

(* Initialize LSP session *)
let init_lsp_session config workspace_root =
  let in_fd_read, in_fd_write = Unix.pipe () in
  let out_fd_read, out_fd_write = Unix.pipe () in

  let pid =
    Unix.create_process config.lsp_command config.lsp_args in_fd_read
      out_fd_write Unix.stderr
  in
  Unix.close in_fd_read;
  Unix.close out_fd_write;

  let in_channel = Unix.in_channel_of_descr out_fd_read in
  let out_channel = Unix.out_channel_of_descr in_fd_write in

  let init_request =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int 1);
        ("method", `String "initialize");
        ( "params",
          `Assoc
            [
              ("processId", `Int (Unix.getpid ()));
              ("rootUri", `String ("file://" ^ workspace_root));
              ( "workspaceFolders",
                `List
                  [
                    `Assoc
                      [
                        ("uri", `String ("file://" ^ workspace_root));
                        ("name", `String "workspace");
                      ];
                  ] );
              ( "capabilities",
                `Assoc
                  [
                    ( "textDocument",
                      `Assoc
                        [
                          ( "definition",
                            `Assoc [ ("dynamicRegistration", `Bool true) ] );
                          ( "references",
                            `Assoc [ ("dynamicRegistration", `Bool true) ] );
                          ( "documentSymbol",
                            `Assoc [ ("dynamicRegistration", `Bool true) ] );
                          ( "semanticTokens",
                            `Assoc
                              [
                                ( "requests",
                                  `Assoc
                                    [
                                      ("full", `Bool true); ("range", `Bool true);
                                    ] );
                                ( "tokenTypes",
                                  `List
                                    [
                                      `String "keyword";
                                      `String "string";
                                      `String "number";
                                    ] );
                                ("tokenModifiers", `List []);
                                ("formats", `List [ `String "relative" ]);
                              ] );
                        ] );
                  ] );
            ] );
      ]
  in

  send_lsp_message out_channel init_request;
  let init_response = read_until_response in_channel 1 in
  let legend =
    init_response |> Yojson.Safe.from_string
    |> Yojson.Safe.Util.member "result"
    |> Yojson.Safe.Util.member "capabilities"
    |> Yojson.Safe.Util.member "semanticTokensProvider"
    |> Yojson.Safe.Util.member "legend"
    |> Yojson.Safe.Util.to_assoc
    |> List.map (fun (x, y) ->
           (x, Yojson.Safe.Util.to_list y |> List.map Yojson.Safe.Util.to_string))
  in

  let initialized =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("method", `String "initialized");
        ("params", `Assoc []);
      ]
  in
  send_lsp_message out_channel initialized;

  (pid, in_channel, out_channel, legend)

(* Open file in LSP *)
let open_file_in_lsp config filename out_channel =
  let file_content =
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content
  in

  let file_uri = "file://" ^ filename in
  let did_open =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("method", `String "textDocument/didOpen");
        ( "params",
          `Assoc
            [
              ( "textDocument",
                `Assoc
                  [
                    ("uri", `String file_uri);
                    ("languageId", `String config.language_id);
                    ("version", `Int 1);
                    ("text", `String file_content);
                  ] );
            ] );
      ]
  in
  send_lsp_message out_channel did_open;
  file_uri

let request_form ?(request = "textDocument/definition") file_uri line char =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", `Int 500);
      ("method", `String request);
      ( "params",
        `Assoc
          [
            ("textDocument", `Assoc [ ("uri", `String file_uri) ]);
            ( "position",
              `Assoc [ ("line", `Int line); ("character", `Int char) ] );
            ("context", `Assoc [ ("includeDeclaration", `Bool true) ]);
          ] );
    ]

let prepare_class_hierarchy in_channel out_channel file line col =
  let _file_uri = open_file_in_lsp python_config file out_channel in
  let request =
    `Assoc [
                        ("jsonrpc", `String "2.0");
                        ("id", `Int 500);
                        ("method", `String "textDocument/prepareCallHierarchy");
                        ("params", `Assoc [
                          ("textDocument", `Assoc [("uri", `String file)]);
                          ("position", `Assoc [("line", `Int line); ("character", `Int col)]);])]
  in
  send_lsp_message out_channel request;
  read_until_response in_channel 500
  |> Yojson.Safe.from_string
  |> Yojson.Safe.Util.member "result"
  |> Yojson.Safe.Util.to_list |> List.hd

let get_incoming_calls in_channel out_channel file line col =
  let hierarchy =
    prepare_class_hierarchy in_channel out_channel file line col
  in
  let request =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int 500);
        ("method", `String "callHierarchy/incomingCalls");
        ( "params",
          `Assoc
            [
              ("textDocument", `Assoc [ ("uri", `String file) ]);
              ("item", hierarchy);
            ] );
      ]
  in
  send_lsp_message out_channel request;
  let response = read_until_response in_channel 500 in
  response |> Yojson.Safe.from_string
  |> Yojson.Safe.Util.member "result"
  |> fun x -> match x with `List _ -> Yojson.Safe.Util.to_list x
| _ -> []

type location = { line : int; character : int }
type range = { start : location; end_ : location }

type node = {
  name : string;
  uri : string;
  from : location;
  fromRanges : range list;
}
let getfile fpath =
let str = Fpath.to_string fpath in
if (String.sub str 0 5) = "file:" then String.sub str 5 (String.length str - 5)
else str

let extract_vertex_from_out node =
  let from = Yojson.Safe.Util.member "from" node in
  let _from_ranges =
    Yojson.Safe.Util.member "fromRanges" node |> Yojson.Safe.Util.to_list
  in
  let str =
    from |> Yojson.Safe.Util.member "name" |> Yojson.Safe.Util.to_string
  in
  let file =
    from |> Yojson.Safe.Util.member "uri" |> Yojson.Safe.Util.to_string |> Fpath.v
  in
  let pos =
    from |> Yojson.Safe.Util.member "range"  |> Yojson.Safe.Util.member "start" |> fun x ->
    Pos.{bytepos = 0;
      line = Yojson.Safe.Util.member "line" x |> Yojson.Safe.Util.to_int;
      column =
        Yojson.Safe.Util.member "character" x |> Yojson.Safe.Util.to_int;
        file;
    }
  in
  Tok.{str; pos}

let add_to_to_tree_from_v in_channel out_channel tree (v: Tok.location)=
let results = get_incoming_calls in_channel out_channel (getfile v.pos.file) v.pos.line v.pos.column 
 in
results  |> List.map  extract_vertex_from_out|> List.fold_left_map (fun _ x -> G.add_vertex tree x; G.add_edge tree v x; ((),x)) ()
  
let add_closed in_channel out_channel tree v =
let rec loop remining  =
if List.is_empty remining then ()
else let new_head = List.hd remining in
let _, new_heads = add_to_to_tree_from_v in_channel out_channel tree new_head in
loop ((List.tl remining) @ new_heads) in
loop v
(* let get all_incomming in_channel out_channel file line col  = *)
(* let rec loop acc (remaining :node list) = *)
(* if List.is_empty remaining then acc *)
(* else let *)
(* {name; uri; from = {}; fromRanges; } = List.hd remaining in *)
(* let new_incomming = get_incoming_calls in_channel out_channel uri line col in *)
(* let new_acc = if List.is_empty new_incomming then acc *)
(* else let new_node = {name; uri; location = (line,col)} in *)

let parse_definition_response response =
  try
    let json = Yojson.Safe.from_string response in
    let result = Yojson.Safe.Util.member "result" json in
    match result with
    | `List [ def ] ->
        let uri =
          Yojson.Safe.Util.member "uri" def |> Yojson.Safe.Util.to_string
        in
        let file_path = String.sub uri 7 (String.length uri - 7) in
        let range = Yojson.Safe.Util.member "range" def in
        let start = Yojson.Safe.Util.member "start" range in
        let line =
          Yojson.Safe.Util.member "line" start |> Yojson.Safe.Util.to_int
        in
        let char =
          Yojson.Safe.Util.member "character" start |> Yojson.Safe.Util.to_int
        in
        Some (file_path, line, char)
    | _ -> None
  with
  | _ -> None

let get_definition file_uri line col in_channel out_channel =
  let form = request_form file_uri (line - 1) col in
  send_lsp_message out_channel form;
  let result = read_until_response in_channel 500 in
  result |> parse_definition_response

let read_sem_token file_name legend (tokens, line, col) converters
    (a, b, c, d, _e) in_channel out_channel =
  let file = Fpath.v file_name in
  let newline = line + a in
  let new_col = if a = 0 then b + col else b in
  let start = converters.Pos.linecol_to_bytepos_fun (newline, new_col) in
  let end_ = start + c - 1 in
  let name = Range.(content_at_range file { start; end_ }) in
  let t = List.nth legend d in
  let def =
    if t = "function" then
      get_definition
        (String.concat "" [ "file://"; file_name ])
        newline new_col in_channel out_channel
    else None
  in
  ((name, (newline - 1, new_col), c, t, def) :: tokens, newline, new_col)

(* Retrieve semantic tokens from LSP *)

let get_semantic_tokens conf file_name in_channel out_channel legend =
  let file =
    match Fpath.of_string file_name with
    | Ok f -> f
    | Error _ -> failwith "Invalid file path"
  in
  let _converters = Pos.full_converters_large file in
  let types = Option.value ~default:[] @@ Assoc.find_opt "tokenTypes" legend in
  let file_uri = open_file_in_lsp conf file_name out_channel in
  let def_request =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int 500);
        ("method", `String "textDocument/semanticTokens/full");
        ( "params",
          `Assoc [ ("textDocument", `Assoc [ ("uri", `String file_uri) ]) ] );
      ]
  in
  send_lsp_message out_channel def_request;
  let response =
    read_until_response in_channel 500
    |> Yojson.Safe.from_string
    |> Yojson.Safe.Util.member "result"
    |> Yojson.Safe.Util.member "data"
    |> Yojson.Safe.Util.to_list
    |> List.map Yojson.Safe.Util.to_int
  in
  let rec loop (acc, line, col) list =
    match list with
    | a :: b :: c :: d :: e :: rest ->
        let x =
          read_sem_token file_name types (acc, line, col) _converters
            (a, b, c, d, e) in_channel out_channel
        in
        loop x rest
    | _ -> acc
  in
  loop ([], 1, 0) response

