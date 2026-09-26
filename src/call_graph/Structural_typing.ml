module G = AST_generic

type 't signature = {
  arity : int;
  parameters : 't option list option;
  return : 't option;
}

type 't equal_type = required:'t -> candidate:'t -> bool option

(* Without the receiver, present on impls but not on interface decls. *)
let method_arity ~(lang : Lang.t) (entity : G.entity option)
    (fdef : G.function_definition) : int =
  Receiver.arity lang ~is_method:(Receiver.is_method fdef)
    ~is_static:(Receiver.is_static entity)
    (Tok.unbracket fdef.G.fparams)

(* Only a definite type mismatch (both sides a simple named type with
   different identity keys) rejects; either side unknown stays compatible,
   so the match degrades to name and arity where types are absent or
   complex. The key is the one the declaring file's own bindings give the
   type, so two packages that both declare [Service] compare unequal. *)
let types_compatible (type declared) ~(equal_type : declared equal_type)
    (required : declared option) (candidate : declared option) : bool =
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
let returns_compatible (type declared) ~(equal_type : declared equal_type)
    (required : declared signature) (candidate : declared signature) : bool =
  types_compatible ~equal_type required.return candidate.return

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

let params_of (fdef : G.function_definition) : G.parameter list =
  match Tok.unbracket fdef.G.fparams with
  | G.ParamReceiver _ :: rest -> rest
  | params -> params

let params_trustworthy (fdef : G.function_definition) : bool =
  List.for_all
    (fun (param : G.parameter) ->
      match param with
      | G.Param { G.pname = Some (name, _); _ } -> not (untrustworthy_pname name)
      | _ -> true)
    (params_of fdef)

(* The result lists the identity key of each parameter's type in position
   order, with [None] for a parameter whose type is not a simple named
   type. *)
let param_types (type declared) ~(declared : G.type_ -> declared)
    (fdef : G.function_definition) : declared option list =
  List.map
    (fun (param : G.parameter) ->
      match param with
      | G.Param { G.ptype; _ } -> Option.map declared ptype
      | _ -> None)
    (params_of fdef)

let params_compatible (type declared) ~(equal_type : declared equal_type)
    (required : declared signature) (candidate : declared signature) : bool =
  (* Skip when either side's param parse is untrustworthy (unnamed-param
     decl garble): fall back to name+arity+return only. *)
  match (required.parameters, candidate.parameters) with
  | Some required_types, Some candidate_types ->
      Int.equal (List.length required_types) (List.length candidate_types)
      && List.for_all2 (types_compatible ~equal_type) required_types
           candidate_types
  | None, _
  | _, None ->
      true

let signature (type declared) ~(lang : Lang.t)
    ~(declared : G.type_ -> declared) (entity : G.entity option)
    (fdef : G.function_definition) : declared signature =
  {
    arity = method_arity ~lang entity fdef;
    parameters =
      (if params_trustworthy fdef then Some (param_types ~declared fdef)
       else None);
    return = Option.map declared fdef.G.frettype;
  }

(* [required] is satisfied by [candidate]: same arity, no definite
   return-type mismatch, and no definite param-type mismatch. *)
let satisfies (type declared) ~(equal_type : declared equal_type)
    ~(required : declared signature) (candidate : declared signature) : bool =
  Int.equal required.arity candidate.arity
  && returns_compatible ~equal_type required candidate
  && params_compatible ~equal_type required candidate
