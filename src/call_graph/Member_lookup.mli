type relation =
  | Extends of {
      constructed : bool;
      virtual_base : bool;
    }
  | Implements
  | Mixin
  | Embedded
  | Included
  | Prepended

type 'c parent =
  | Resolved of relation * 'c
  | Unresolved of relation

type superclass =
  | Written_as_extends
  | Carrying_constructor_arguments
  | First_parent_if_class

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
  | Cpp_member_lookup
  | Rust_method_probing

type 'c candidate = {
  cls : 'c;
  hides : 'c list;
  paths : int;
}

type 'c base =
  | Base of {
      cls : 'c;
      virtual_base : bool;
      bases : 'c base list;
    }
  | Unknown_base

type 'c level =
  | Candidates of 'c candidate list
  | Base_subobjects of 'c base list
  | Unknown_classes

type 'c lookup_order = {
  order : 'c list;
  complete : bool;
  levels : 'c level list;
  super_levels : 'c level list;
}

type ('c, 'a) selection =
  | Selected of 'c * 'a list
  | Ambiguous
  | Undefined
  | Unknown

val super_follows_receiver_order : strategy -> bool
val hides_inherited_overloads : strategy -> bool
val level_classes : 'c level -> 'c list
val levels_after : equal:('c -> 'c -> bool) -> 'c -> 'c level list -> 'c level list

val select :
  equal:('c -> 'c -> bool) ->
  defines:('c -> 'a list) ->
  overrides:(nearer:'a -> farther:'a -> bool) ->
  overload_key:('a -> int) ->
  declared_only:('a -> bool) ->
  is_static_member:('a -> bool) ->
  accumulate:bool ->
  'c level list ->
  ('c, 'a) selection

val lookup_order :
  strategy ->
  equal:('c -> 'c -> bool) ->
  hash:('c -> int) ->
  parents:('c -> 'c parent list list) ->
  is_interface:('c -> bool) ->
  is_external:('c -> bool) ->
  dereferences:('c -> 'c option) ->
  'c ->
  'c lookup_order
