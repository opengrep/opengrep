module G = AST_generic

type method_ = {
  name : string;
  entity : G.entity option;
  fdef : G.function_definition;
}

type equal_type = required:G.type_ -> candidate:G.type_ -> bool option

(* Without the receiver, present on impls but not on interface decls. *)
let method_arity ~(lang : Lang.t) (method_ : method_) : int =
  Receiver.arity lang ~is_method:(Receiver.is_method method_.fdef)
    ~is_static:(Receiver.is_static method_.entity)
    (Tok.unbracket method_.fdef.G.fparams)

(* Only a definite type mismatch (both sides a simple named type with
   different identity keys) rejects; either side unknown stays compatible,
   so the match degrades to name and arity where types are absent or
   complex. The key is the one the declaring file's own bindings give the
   type, so two packages that both declare [Service] compare unequal. *)
let types_compatible ~(equal_type : equal_type) (required : G.type_ option)
    (candidate : G.type_ option) : bool =
  match (required, candidate) with
  | Some required, Some candidate ->
      Option.value (equal_type ~required ~candidate) ~default:true
  | _ -> true

(* The result is the identity key of the single return type, and [None]
   when the function has no return value or the return value has no simple
   named type (a Go multiple return, a function type, and so on).

   We compare RETURN types, not parameters: tree-sitter-go misparses an
   unnamed-param interface decl ([Group(string, func(RouteRegister),
   ...)]) by shifting the [name type] pairing — it reads [string] as
   the parameter NAME and the following token as its type — so a
   genuine implementation's params disagree with its own interface's
   garbled params. A return position is always a bare type (no
   [name type] ambiguity), so its key is trustworthy on both the decl
   and the impl. *)
let returns_compatible ~(equal_type : equal_type) (required : method_)
    (candidate : method_) : bool =
  types_compatible ~equal_type required.fdef.G.frettype
    candidate.fdef.G.frettype

(* tree-sitter-go misparses an UNNAMED-param interface decl when a
   composite-type keyword ([func]/[map]/[chan]) follows a bare type
   ([Group(string, func(I), ...)]): it reads the preceding token as the
   param NAME and shifts the type, so the garbled decl's param types
   disagree with a genuine impl's. Verified: bare, capitalized,
   qualified ([io.Writer]), slice, struct{} and fully-unnamed params
   all parse correctly ([pname = None]); only the composite-keyword
   case garbles.

   Crucially, when it garbles the trigger keyword itself lands as a
   [pname] ([func]/[map]/[chan]) — a Go RESERVED WORD, which can never
   be a legal identifier, so its presence as a param name is a definite
   garble marker. Any method with such a param has its param parse
   treated as untrustworthy and param comparison skipped (return type
   and name+arity still apply), so a real impl of an unnamed-param
   interface is never dropped on garbled data. The predeclared type
   names below ([string]/[int]/...) are defensive: they CAN legally be
   param names, but flagging one only forces the same conservative skip
   (keep the edge), never a wrong rejection. *)
let untrustworthy_pname (name : string) : bool =
  match name with
  (* Reserved words — impossible as identifiers; the garble triggers. *)
  | "func" | "map" | "chan" | "interface" | "struct" | "type" | "range"
  (* Predeclared type names — defensive; a conservative skip at worst. *)
  | "string" | "bool" | "byte" | "rune" | "error" | "any" | "uintptr"
  | "int" | "int8" | "int16" | "int32" | "int64"
  | "uint" | "uint8" | "uint16" | "uint32" | "uint64"
  | "float32" | "float64" | "complex64" | "complex128" -> true
  | _ -> false

let params_of (method_ : method_) : G.parameter list =
  match Tok.unbracket method_.fdef.G.fparams with
  | G.ParamReceiver _ :: rest -> rest
  | params -> params

let params_trustworthy (method_ : method_) : bool =
  List.for_all
    (fun (param : G.parameter) ->
      match param with
      | G.Param { G.pname = Some (name, _); _ } -> not (untrustworthy_pname name)
      | _ -> true)
    (params_of method_)

(* The result lists the identity key of each parameter's type in position
   order, with [None] for a parameter whose type is not a simple named
   type. *)
let param_types (method_ : method_) : G.type_ option list =
  List.map
    (fun (param : G.parameter) ->
      match param with
      | G.Param { G.ptype; _ } -> ptype
      | _ -> None)
    (params_of method_)

let params_compatible ~(equal_type : equal_type) (required : method_)
    (candidate : method_) : bool =
  (* Skip when either side's param parse is untrustworthy (unnamed-param
     decl garble): fall back to name+arity+return only. *)
  if not (params_trustworthy required && params_trustworthy candidate) then
    true
  else
    let required_types = param_types required in
    let candidate_types = param_types candidate in
    Int.equal (List.length required_types) (List.length candidate_types)
    && List.for_all2 (types_compatible ~equal_type) required_types
         candidate_types

(* [iface_m] is satisfied by [concrete_m]: same name, same arity, no
   definite return-type mismatch, and no definite param-type mismatch. *)
let method_satisfies ~(lang : Lang.t) ~(equal_type : equal_type)
    ~(required : method_) (candidate : method_) : bool =
  String.equal required.name candidate.name
  && Int.equal (method_arity ~lang required) (method_arity ~lang candidate)
  && returns_compatible ~equal_type required candidate
  && params_compatible ~equal_type required candidate
