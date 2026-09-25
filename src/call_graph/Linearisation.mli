type placement =
  | Prepended
  | Appended

type 'c parent =
  | Bound of placement * 'c
  | Unbound of placement

type 'c linearisation = {
  order : 'c list;
  complete : bool;
}

val c3 :
  equal:('c -> 'c -> bool) ->
  hash:('c -> int) ->
  parents:('c -> 'c parent list list) ->
  'c ->
  'c linearisation
