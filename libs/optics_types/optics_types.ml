(* src/optics/optics_types.ml *)
module type MONOID = sig
  type t
  val empty  : t
  val append : t -> t -> t
end

type 's traversal = { map_children : ('s -> 's) -> 's -> 's }

type ('s,'a) prism = {
  preview : 's -> 'a option;
  review  : 'a -> 's;
}

type ('s,'a) fold = {
  fold_map :
    'm. (module MONOID with type t = 'm) ->
        ('a -> 'm) -> 's -> 'm;
}

(* convenience helpers used in your code *)
let over_tr (tr : 's traversal) (f : 's -> 's) (s : 's) : 's =
  tr.map_children f s

let preview (p : ('s,'a) prism) (s : 's) : 'a option =
  p.preview s
