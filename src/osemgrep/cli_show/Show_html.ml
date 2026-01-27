module G = AST_generic

let counter = ref 0

let fresh_name () =
  counter := !counter + 1;
  "body" ^ string_of_int !counter

let div ?(id="") ?(style="") ?(css="") ?(js="") body =
  let tag_id = if id = "" then "" else " id=\"" ^ id ^ "\"" in
  let tag_css = if css = "" then "" else " class=\"" ^ css ^ "\"" in
  let tag_js = if js = "" then "" else  " onClick=\"" ^ js ^ "\"" in
  let tag_style = if style = "" then "" else  " style=\"" ^ style ^ "\"" in
  "<div" ^ tag_id ^ tag_css ^ tag_js ^ tag_style ^ ">" ^ body ^ "</div>"

let node ~label ~css ~body : string =
  let node = div ~css:("node-header-box " ^ css) label in
  let header = div ~css:"node-header" node in
  div ~css:"node-and-body" (header ^ body)

let node_and_body ~label ~css ~body ~folded ~comment : string =
  let id = fresh_name () in
  let toggle_symbol = if folded then "+" else "-" in
  let toggle = div ~id:("t" ^ id) ~js:("toggleVisibility('" ^ id ^ "')") ~css:"node-toggle" toggle_symbol in
  let node = div ~css:("node-header-box " ^ css) label in
  let comment = div ~css:"comment" comment in
  let header = div ~css:"node-header" (toggle ^ node ^ comment) in
  let style = if folded then "display:none;" else "" in
  let body = div ~id ~css:"node-body" ~style body in
  div ~css:"node-and-body" (header ^ body)

let in_quotes s =
  "\"" ^ s ^ "\""

type display_node_type =
  Stmt | Expr | WithChildren | Leaf | Label | Ident | Token

type display_node =
  { header : string;
    children : display_node list;
    type_ : display_node_type;
    folded : bool;
    comment : string;
  }

let make_node ?(label=false) ?(comment="") header children =
  let is_stmt s =
    match s with
    | "ExprStmt"
    | "Block"
    | "If"
    | "While"
    | "Return"
    | "DoWhile"
    | "For"
    | "Switch"
    | "Continue"
    | "Break"
    | "Label"
    | "Goto"
    | "Throw"
    | "Try"
    | "WithUsingResource"
    | "Assert"
    | "DefStmt"
    | "DirectiveStmt"
    | "DisjStmt"
    | "OtherStmtWithStmt"
    | "OtherStmt"
    | "RawStmt" -> true
    | _ -> false
  in
  let is_expr e =
    match e with
    | "L"
    | "Container"
    | "Comprehension"
    | "Record"
    | "Constructor"
    | "N"
    | "IdSpecial"
    | "Call"
    | "RegexpTemplate"
    | "New"
    | "Xml"
    | "Assign"
    | "AssignOp"
    | "LetPattern"
    | "DotAccess"
    | "ArrayAccess"
    | "SliceAccess"
    | "Lambda"
    | "AnonClass"
    | "Conditional"
    | "Yield"
    | "Await"
    | "Cast"
    | "Seq"
    | "Ref"
    | "DeRef"
    | "Alias"
    | "LocalImportAll"
    | "Ellipsis"
    | "DeepEllipsis"
    | "DisjExpr"
    | "TypedMetavar"
    | "DotAccessEllipsis"
    | "StmtExpr"
    | "OtherExpr"
    | "RawExpr" -> true
    | _ -> false
  in
  let type_ =
    match label, children with
    | true, _ -> Label
    | _ when header = "token" -> Token
    | _ when is_expr header -> Expr
    | _ when is_stmt header -> Stmt
    | _, [] -> Leaf
    | _ -> WithChildren
  in
  let folded = header = "pinfo" || header = "id_info" in
  { header; children; type_; folded; comment }

let make_leaf (header : string) : display_node =
  make_node header []

let display_node_of_v (v : OCaml.v) =
  let color_ident n =
    match n with
    | { children = [i; j]; _ } ->
        { n with children = [{ i with type_ = Ident }; j] }
    | _ -> n
  in
  let entity_name v =
    match v with
    | OCaml.VSum ("DefStmt", (OCaml.VDict (("name", OCaml.VSum ("EN", (OCaml.VSum ("Id", (OCaml.VTuple (OCaml.VString s :: _) :: _)) :: _))) :: _) :: _)) -> s
    | _ -> ""
  in
  let def_type v =
    match v with
    | OCaml.VSum ("DefStmt", [_; OCaml.VSum (s, _)]) -> s
    | _ -> ""
  in
  let rec aux2 v =
    let make_node ?(label=false) a b = make_node ~label ~comment:(def_type v ^ " " ^ entity_name v) a b in
    match v with
    (* nodef with folded id_info and added color *)
    | OCaml.VSum (("Id" | "PatId") as c, [a;b]) ->
        let bb = aux b in
        make_node c [color_ident (aux a); { bb with folded = true }]
    | OCaml.VSum ("New", [a;b;c;d]) ->
        let cc = aux c in
        make_node "New" [aux a; aux b; { cc with folded = true }; aux d]
    (* generic node *)
    | OCaml.VUnit -> make_leaf "token"
    | OCaml.VBool v -> make_leaf (string_of_bool v)
    | OCaml.VFloat v -> make_leaf (string_of_float v)
    | OCaml.VChar v -> make_leaf (String.of_seq (Seq.return v))
    | OCaml.VString v -> make_leaf (in_quotes v)
    | OCaml.VInt v -> make_leaf (Int64.to_string v)
    | OCaml.VTuple xs -> make_node "(,)" (List.map aux xs)
    | OCaml.VDict xs ->
        make_node "{;}" (List.map (fun (k, v) -> make_node ~label:true k [aux v]) xs)
    | OCaml.VSum (s, xs) -> make_node s (List.map aux xs)
    | OCaml.VVar (s, i64) -> make_node s [make_leaf (Int64.to_string i64)]
    | OCaml.VArrow _ -> make_leaf "UNSUPPORTED (VArrow)"
    | OCaml.VNone -> make_leaf "None"
    | OCaml.VSome v -> make_node "Some" [aux v]
    | OCaml.VRef v -> make_node "ref" [aux v]
    | OCaml.VList xs -> make_node "[;]" (List.map aux xs)
    | OCaml.VTODO _ -> make_leaf "UNSUPPORTED (VTODO)"
  and aux v =
    match v with
    | OCaml.VSum (a, [OCaml.VTuple ((_ :: _) as xs)]) -> aux2 (OCaml.VSum (a, xs))
    | _ -> aux2 v
  in
  aux v

let rec html_of_display_node d : string =
  let header_box_class =
    match d.type_ with
    | Leaf -> "node-type-leaf"
    | Label -> "node-type-label"
    | WithChildren -> "node-type-children"
    | Expr -> "node-type-expr"
    | Stmt -> "node-type-stmt"
    | Ident -> "node-type-ident"
    | Token -> "node-type-token"
  in
  let children =
    List.map html_of_display_node d.children
    |> String.concat ""
  in
  match d.type_ with
  | Leaf | Ident | Token ->
      node ~css:header_box_class ~label:d.header ~body:children
  | _ ->
      node_and_body ~css:header_box_class ~label:d.header ~body:children ~folded:d.folded ~comment:d.comment

let html_init = {|
<html><head><style>
body {
  font-family:sans-serif;
  margin: 0px;
}
.node-and-body {
  margin-bottom:5px;
}
.node-header-box {
  display:inline-block;
  border-radius:3px;
  padding:2px 7px;
}
.node-body {
  margin-left:10px;
  padding-left:10px;
  padding-top:5px;
  border-left:1px dotted #333;
}
.node-toggle {
  display:inline-block;
  border:1px dotted #555;
  padding:2px;
  border-radius:3px;
  cursor:pointer;
  background-color:#eee;
  width:10px;
  text-align:center;
}
.node-type-stmt {
  background-color:#faa;
  border:1px solid #d22;
  color:#522;
}
.node-type-expr {
  background-color:#afa;
  border:1px solid #2d2;
  color:#252;
}
.node-type-ident {
  background-color:#fe2;
  border:1px solid #961;
  color:#321;
}
.node-type-children {
  background-color:#bcf;
  border:1px solid #34d;
  color:#225;
}
.node-type-label {
  color:#222;
  font-weight:bold;
}
.node-type-leaf {
  color:#222;
  background-color:#eee;
  border:1px solid #888;
}
.node-type-token {
  color:#777;
  background-color:f6f6f6;
  border:1px dotted #aaa;
  display:none;
}
.folded {
  display:none;
}
.comment {
  display:inline-block;
  margin-left:8px;
  color: #888;
  font-style: italic;
}
.doc-body {
  padding:5px;
}
.doc-header {
  background-color:#eee;
  border-bottom:1px solid #ccc;
  padding:7px;
}
</style><script>
function toggleVisibility(id) {
  const element = document.getElementById(id);
  const toggle = document.getElementById('t' + id);
  if (element.style.display == "none") {
    element.style.display = "block";
    toggle.innerHTML = "-";
  } else {
    element.style.display = "none";
    toggle.innerHTML = "+";
  }
}
function toggleToks() {
  const checkbox = document.getElementById("toggleTokens");
  // Select all elements with the class "box"
  const elems = document.querySelectorAll(".node-type-token");
  elems.forEach(el => {
    if (checkbox.checked) {
      el.style.display = "inline-block";   // hide when checked
    } else {
      el.style.display = "none";  // show when unchecked
    }});
}
function collapseAll() {
  const elems = document.querySelectorAll(".node-body");
  elems.forEach(el => { el.style.display = "none"; });
  const elemsTog = document.querySelectorAll(".node-toggle");
  elemsTog.forEach(el => { el.innerHTML = "+"; });
}
function expandAll() {
  const elems = document.querySelectorAll(".node-body");
  elems.forEach(el => { el.style.display = "block"; });
  const elemsTog = document.querySelectorAll(".node-toggle");
  elemsTog.forEach(el => { el.innerHTML = "-"; });
}
</script></head><body>
<div class="doc-header">
<button onClick="expandAll();">Expand all</button>
<button onClick="collapseAll();">Collapse all</button>
<input type="checkbox" id="toggleTokens" onChange="toggleToks();">Show token placeholders</input>
</div>
<div class="doc-body">
|}

let html_end = "</div></body></html>"

let generate_html (v : OCaml.v) : string =
  html_init ^
  (v |> display_node_of_v |> html_of_display_node) ^
  html_end
