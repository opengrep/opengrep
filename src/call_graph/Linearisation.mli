type relation =
  | Extends of { constructed : bool }
  | Implements
  | Mixin
  | Embedded
  | Included
  | Prepended

type 'c parent =
  | Bound of relation * 'c
  | Unbound of relation

type superclass =
  | Written_as_extends
  | Carrying_constructor_arguments
  | Of_class_kind

type mixins =
  | Applied_in_the_chain
  | Flattened_into_the_class

type strategy =
  | C3 of { bases_listed_most_base_first : bool }
  | Scala_class_linearisation
  | Ruby_ancestor_chain
  | Single_inheritance of {
      superclass : superclass;
      interface_bodies_inherited : bool;
      mixins : mixins;
    }
  | Go_embedding_promotion

type 'c candidate = {
  cls : 'c;
  hides : 'c list;
  paths : int;
}

type 'c tier =
  | Candidates of 'c candidate list
  | Unknown_classes

type 'c linearisation = {
  order : 'c list;
  complete : bool;
  tiers : 'c tier list;
  super_tiers : 'c tier list;
}

type ('c, 'a) selection =
  | Selected of 'c * 'a list
  | Ambiguous
  | Undefined
  | Unknown

val follows_receiver : strategy -> bool
val after : equal:('c -> 'c -> bool) -> 'c -> 'c tier list -> 'c tier list

val select :
  equal:('c -> 'c -> bool) ->
  defines:('c -> 'a list) ->
  overrides:(nearer:'a -> farther:'a -> bool) ->
  declared_only:('a -> bool) ->
  accumulate:bool ->
  'c tier list ->
  ('c, 'a) selection

val linearise :
  strategy ->
  equal:('c -> 'c -> bool) ->
  hash:('c -> int) ->
  parents:('c -> 'c parent list list) ->
  is_interface:('c -> bool) ->
  defined_outside:('c -> bool) ->
  'c ->
  'c linearisation
