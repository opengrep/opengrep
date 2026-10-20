(* Python has one namespace. A class binds once its definition is visited, so
   a base class of the same name refers to the earlier binding, as in
   [class Foo(Foo):]. A class does not rebind an imported name: rules rely on
   an import keeping its resolution, as an assignment does not rebind it
   either (see [lookup_for_implicit_assign_opt] in Naming_AST), e.g. for a
   fallback class in the [except ImportError] of [from lxml import etree]. *)
include Lang_namespaces.Single

let class_rebinds_import = Lang_namespaces.No_scope
let class_in_scope_in_its_definition = false
