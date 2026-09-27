type verdict = Feasible | Infeasible | Unknown

type trace_step =
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
val equal_trace_step : trace_step -> trace_step -> bool
val hash_trace_steps : trace_step list -> int
val literals : state -> (IL.exp * bool) list

val check :
  Lang.t ->
  IL.fun_cfg ->
  index ->
  entry:state ->
  trace_step list ->
  verdict * state option list
