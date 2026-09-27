type verdict = Feasible | Infeasible | Unknown

type anchor =
  | Entry
  | Token of Tok.t
  | Range of Tok.location * Tok.location
  | Call of IL.exp
  | Exit

type index
type state

val index : IL.fun_cfg -> index
val entry_state : (IL.name * AST_generic.svalue) list -> state
val value : Lang.t -> state -> IL.exp -> AST_generic.svalue
val refutes : state -> (IL.exp * bool) list -> bool
val equal_state : state -> state -> bool
val equal_anchor : anchor -> anchor -> bool
val hash_anchors : anchor list -> int
val literals : state -> (IL.exp * bool) list

val check :
  Lang.t ->
  IL.fun_cfg ->
  index ->
  entry:state ->
  anchor list ->
  verdict * state option list
