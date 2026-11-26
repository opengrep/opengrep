# 4. Per-language quirks

Every language has its own definition of a class, a method, a scope and
the function that a call runs.  projidx carries these definitions as
fields of `Index_lang_rules.t` (`src/project_index/Index_lang_rules.mli`).
This document describes, language by language, the constructs that
require such a field and the code that handles them.

## The Index_lang_rules.t fields

`Index_lang_rules.default` gives every field its value for a language
without an entry of its own; the third column lists the entries whose
value differs.  `for_lang` maps JavaScript to the TypeScript entry, Hack
to the PHP entry, and Move on Aptos and Move on Sui to `module_scoped`.
Cairo's entry extends Rust's, so Cairo also carries every value that
Rust sets.

| Field | What it does | Set to a non-default value by |
|---|---|---|
| `is_init_file` | Recognise package-init files.  Gates the re-export map. | Python (`__init__.py`) |
| `is_stub_file` | Recognise stub files.  A stub file gets no file scope, and its functions, classes and file record are left out of the index. | Python (`.pyi`) |
| `project_scope_admits` | The definitions that enter a project-wide scope. | Lua (a definition that has an entity and no `static` attribute) |
| `rewrite_module_path` | Rewrite the path-derived module path.  `foo/bar/x.py` becomes `foo.bar.x`. | Python (drops a trailing `/__init__`), Lua (drops a trailing `/init`), OCaml (keeps the basename only, capitalised) |
| `module_path_from_ast` | Extract the file's module or namespace from its AST, overriding the path-derived module path. | Java, Kotlin, C#, Scala (`package`, file-scoped `namespace`), PHP, Hack, C, C++ (the first `namespace`, else the global namespace), Clojure (`(ns ...)`) |
| `normalize_import_specifier` | Rewrite a raw import specifier before it becomes a module key. | C, C++ (strip `.h`, `.hpp`, `.hh`, `.hxx`) |
| `specifiers_name_files` | An import specifier is a file path, resolved against the files of the project, not a module path. | TypeScript/JavaScript, Lua, Dart, Bash, Circom, Solidity |
| `specifiers_are_uris` | A specifier is a URI: one without a scheme is relative to the importing file, one with a scheme goes through the module paths. | Dart |
| `bare_specifiers_relative_first` | A specifier without a leading dot is looked up relative to the importing file first, then through the module paths. | Bash, Circom |
| `file_imports_of_expr` | Recognise the expression statements that run files in the current scope, including the operands of `&&`, `\|\|` and `!`; each binds every top-level definition of its file. | Bash (`source f`, `. f`) |
| `library_parts_of_ast` | The specifiers of the files that the `part` directives of a file add to its library. | Dart |
| `specifier_candidates` | The file paths that a specifier may refer to. | TypeScript/JavaScript (source extensions, `index` files), Lua (dots as directory separators, `.lua`, `init.lua`) |
| `path_import_binds_all_names` | `import "path";` without an alias binds every top-level definition of the file. | Solidity |
| `wildcard_import_omits_underscore_names` | A wildcard import of a module without `__all__` leaves out the definitions whose identifier begins with an underscore.  Default `true`. | `false`: Bash, Circom, Solidity, Cairo, Move, OCaml, R, QL, Java, Kotlin, C#, C++, Scala, VB |
| `imports_bind_in_statement_order` | An import binds from its statement onward and rebinds an identifier that an earlier import bound: of the imports before a class that bind its parent through a wildcard import or through re-exports, the last one binds it, and of the wildcard re-exports of a module that bind a class, the last one binds it.  Elsewhere imports that bind the identifier to different classes bind it through neither. | Python |
| `crate_manifest` | The manifest whose `[package]` name heads the module path of the files under it. | Rust (`Cargo.toml`), Cairo (`Scarb.toml`) |
| `crate_root_stems` | The stems of the files directly under `src` that are the root module of a crate. | Rust (`lib`, `main`), Cairo (`lib`) |
| `class_dunders_from_decorators` | Python `@dataclass` → `[__init__]` (honouring `init=False`), `__replace__`, and `__hash__` when `frozen=True`. | Python |
| `class_dunders_from_extends` | Python `class X(NamedTuple)` → `[__init__; __new__; __iter__; ...]`. | Python |
| `synth_call_dunders` | Python `X = Enum(...)` / `namedtuple(...)` / `NewType(...)` are class-defining calls. | Python |
| `inner_class_from_call` | `class X(namedtuple("Y", ...))`: extract Y and its dunders. | Python |
| `class_body_synth_methods` | Ruby `attr_reader :foo` (also `attr_writer`, `attr_accessor`, `delegate`) declares a method with no FuncDef. | Ruby |
| `class_body_extra_parents` | Parents declared outside the extends clause, each with its position in the resolution order. | Ruby (`include`, `extend` appended, `prepend` prepended), Go (embedded types), PHP, Hack (trait `use`), Scala, Dart (mixins, last applied first) |
| `superclass_position` | Whether the superclass comes before or after the mixins among the parents. | `Superclass_after_mixins`: Scala, Dart |
| `bases_listed_most_base_first` | The extends list is reversed before linearisation, because it lists the most base-like class first. | Solidity |
| `mixin_extends_lists_constraints` | The extends clause of a mixin (`G.Trait`) lists the classes it may be applied to: they enter the mixin's own resolution order, not the order of a class that applies the mixin. | Dart (`mixin M on B`) |
| `superclass_is_never_a_mixin` | The superclass position takes a class: a mixin (`G.Trait`) of the same identifier is not a candidate for it in the lookup by scope and by simple identifier. | Dart |
| `class_body_singleton_methods` | Which methods of a module are also callable on the module itself. | Ruby (`extend self`, `module_function`) |
| `extract_wrapper` / `wrapper_dunders` | Python PEP-681 `@dataclass_transform()`. | Python |
| `walks_inheritance` | Run `Mro.inherit_into_type_state` (C3 resolution orders, inherited methods attached to subclasses) and record a module definition as a class. | Python, Ruby, Go, TypeScript/JavaScript, PHP, Hack, Scala, Rust, Cairo, Java, Kotlin, C#, C++, Apex, Swift, Lua, Solidity, Crystal, Dart, VB |
| `method_member_scope` | Which members of its class a bare identifier in a method refers to. | `Members_in_class_scope`: Solidity; `Inherited_members_after_lexical_scope`: Dart |
| `has_reexports` | Enable the re-export pass. | Python (`__init__.py`), Rust, Cairo (`pub use`) |
| `reexport_source` | Where re-exports come from: the imports of an init file, or the import directives with the `Public` attribute. | `Reexports_from_public_directives`: Rust, Cairo |
| `include_anonymous_funcs` | Lambdas and blocks are call-graph vertices.  Default `true`. | `false`: Python, Ruby, Go, TypeScript/JavaScript, PHP, Hack, Java, Kotlin, C#, C++, Crystal |
| `assigned_function_defines_identifier` | Assigning a function value to an identifier defines the function under that identifier in the scope of the assignment, in the per-file and per-project scopes. | R |
| `unqualified_scope` | Which scope builder binds the unqualified identifiers of a file.  Default `Per_file`. | `Per_constant_path`: Ruby, Crystal; `Per_go_package`: Go; `Per_module`: TypeScript/JavaScript; `Per_namespace`: PHP, Hack, Elixir, Move; `Per_package`: Scala, Java, Kotlin, C#, VB; `Per_crate`: Rust, Cairo; `Per_translation_unit`: C, C++; `Per_project`: Apex, Swift, Lua, Julia, R.  No entry sets `Per_directory`, which `Pipeline` builds as `Per_file`. |
| `precedence` | The rank of each binding kind.  Where several kinds bind one identifier at file scope, the binding of the highest rank applies. | Java (`Single_import` above `Own_definition`), Scala (`Package_member` lowest, `Own_definition` highest) |
| `own_package_members_kind` | The binding kind of a definition from another file of the same package or enclosing scope. | Scala (`Package_member`) |
| `namespaces_nest` | The members of every enclosing namespace bind in a file's namespace. | C#, VB |
| `relative_module_names` | Leading path segments that refer to a module relative to the current one. | Rust, Cairo (`crate`, `self`, `super`) |
| `import_head_may_be_own_module` | The first segment of an import path may refer to a module defined in the importing file. | Rust, Cairo |
| `package_directive_is_namespace` | The language's `Package`/`PackageEnd` directives are qn scopes (namespace blocks / package clauses), not the file's module identity. | TypeScript/JavaScript, PHP, Hack, Scala, Java, Kotlin, C#, C++, C |
| `module_definition_is_namespace` | A module definition (`G.ModuleDef`) opens a namespace scope. | Elixir, Move, VB |
| `object_members_bind_in_namespace` | The members of an object (`G.Object`) bind in its enclosing namespace. | VB |
| `dict_literal_is_object_definition` | A variable assigned a dictionary literal is recorded as an object with the literal's fields as members. | Lua |
| `module_is_returned_value` | A file's module is the table literal that its final `return` statement returns. | Lua |
| `companion_object_has_own_name` | An `object` gets its own qn (suffix `$`), separate from its companion class. | Scala |
| `unaliased_import_binds` | What an import without an alias binds.  Default `First_segment_binds`. | `Last_segment_binds`: PHP, Hack, Cairo, Elixir, Move; `Every_name_binds` (every top-level definition of the module): QL |
| `hiding_alias` | The alias that hides an imported identifier instead of binding it. | Scala (`_`) |
| `class_identity_is_constant_path` | Class identity is its constant path, file-independent: drops the file-path prefix from class qns. | Ruby, Crystal (class reopening) |
| `discover_project` | Read the project's configuration files for excluded paths and module path mappings. | TypeScript/JavaScript (`tsconfig` `exclude` and `paths`), Lua (every specifier maps under the project root), Dart (`package:<name>/` maps to `lib/`, with `<name>` read from `pubspec.yaml`) |
| `class_def_reshape` | Coerce a definition into `ClassDef` shape for the symbol collector.  (Ruby `module ... end` is handled by a dedicated `ModuleDef` branch in the symbol collector, not by this hook.) | Go (`type T struct/interface`, and a class with no fields for every other `type T ...`), Rust, Cairo (`impl Foo { ... }`, through `Visit_function_defs.class_scope_of_definition`) |
| `strip_field_sigil` | Normalise field names: drop the `$` so `$this->x` and promoted ctor params share one field namespace. | PHP, Hack |
| `class_constructor_synth_fields` | Extract fields declared by constructor parameters. | TypeScript/JavaScript (`constructor(private x: T)`) |
| `ctor_param_promotion` | Register every typed constructor param as a candidate field. | PHP, Hack (PHP 8 property promotion) |
| `interface_dispatch_uses_export_visibility` | Interface dispatch treats an interface with any unexported method as package-private: implementers must share its package. | Go |
| `parent_resolution` | How a parent path is looked up when no single import binds it: in the class's own module only, through the enclosing lexical scopes, or lexically and then by simple identifier across the project (default). | `Parent_in_own_scope`: Go, Rust, Cairo; `Parent_by_lexical_scope`: Ruby, Crystal, C++ |
| `package_clause_of_ast` | The file's `package` clause, which identifies a Go package together with the directory. | Go |
| `method_owner_of_funcdef` | The class that a function definition is a method of, from its receiver. | Go |
| `name_is_exported` | Whether an identifier is visible outside its file or package.  Default: every identifier. | Go (does not begin with a lower-case letter or `_`), Dart (does not begin with `_`) |

## Python

### `__init__.py` and re-exports

Python's `foo/__init__.py` is module `foo`, not `foo.__init__`.  The
file's import statements re-export names: `from .bar import thing`
makes `thing` available as `foo.thing`.  The Python config:

- `is_init_file = endswith "__init__.py"`
- `rewrite_module_path` strips `/__init__`.
- `has_reexports = true` triggers a post-phase that walks each
  init file's imports and records `(module, name) → original_qn`
  mappings.

When a caller writes `from foo import thing` and then `thing()`,
projidx resolves `thing` through the re-export map back to its
original `qn`.  A wildcard import of an init file, `from .models import
*`, is a wildcard re-export of the package
(`Reexports.build_wildcard_reexports`), through which a base class
imported from the package is found (see the parent class lookup below).

### Dataclass / NamedTuple synthesis

`@dataclass` on a class adds synthesised `__init__` (unless
`init=False`) and `__replace__` methods, plus `__hash__` when
`frozen=True`.  `class X(NamedTuple)` adds `__init__`, `__new__`,
`__iter__` and others.  These methods have no AST FuncDef but are call
targets:

```python
@dataclass
class User:
    name: str

u = User(name="alice")   # calls synthesised __init__
```

`class_dunders_from_decorators` and `class_dunders_from_extends`
return the list of dunders to synthesise.  Phase 1 emits
`func_info` entries for each synthesised dunder with a placeholder
fdef (empty body, empty params, arity 0) so call sites resolve.

### PEP-681 `@dataclass_transform`

Frameworks like attrs and pydantic use the PEP-681
`@dataclass_transform()` decorator to declare *their* decorator as a
dataclass-shaped class synthesiser.  Phase 1 detects this via
`extract_wrapper` and synthesises the same dunders on classes
decorated with the wrapper.

### Implicit receivers (`self` / method receiver)

Signatures track parameters by index.  A call site to
`MyClass.__init__(x, y)` passes `x` and `y` as positional args 0 and
1, *not* 1 and 2: implicit receiver dispatch supplies `self`, not the
call expression.  The method body reaches the receiver as `BThis`,
never as a `BArg`.

`Match_tainting_mode.ml` (`is_implicit_receiver` /
`filter_implicit_receiver_params`) therefore strips the implicit
receiver from the param list: Python `self`/`cls` and Go/Rust
`ParamReceiver`.  This is done **in both intra- and interfile
modes** (the `interfile` flag is ignored), so the parameter indices
and the arity of a signature do not depend on the mode that produced
it.  The `BArg` indices then line up with the actuals at every call
site: direct construction, `super().__init__`, and plain method calls.

## Go

### Packages and `Per_go_package` scope

A Go file refers to a definition of another file of its package without
an import: `func Helper()` declared in `foo/util.go` is callable from
`foo/main.go` as bare `Helper()`.  `unqualified_scope` is
`Per_go_package`: `Scope_go` binds an unqualified identifier in
`foo/main.go` to the definitions of every file of `foo/` with the same
`package` clause (`package_clause_of_ast`).

For cross-package references like `pkg.Func()`, `Scope_go.build` maps
the local identifier of each import to the imported package
(`import_aliases`, returned as `module_aliases`), and `pkg.Func`
resolves among the definitions of that package.

### `type T interface { ... }`

Go's tree-sitter parser does **not** emit `ClassDef` for an
interface.  It emits:

```
DefStmt(entity, TypeDef { tbody = NewType (TyRecordAnon (Interface, fields)) })
```

The default `iter_no_id_info` visitor does not recurse into the
`TyRecordAnon` field list.  Two mechanisms make the methods of the
interface members of a class:

- `class_def_reshape` (`go_class_def_reshape`) returns a `ClassDef`
  whose body is the field list for a `TypeDef` of a struct or interface
  record, and a `ClassDef` without fields for every other `type T ...`.
  The symbol collector (`Symbols`) applies it, so the type is recorded
  as a class.
- `Visit_function_defs` walks the field list of a Go interface `TypeDef`
  with `current_class` set to the interface, so each method declaration
  gets `fn_id = [Some Iface; Some method]`.

### Interface embedding

```go
type Authenticator interface {
    Authenticate(ctx, req) error
}

type Client interface {
    Authenticator        // embed
    Name() string
    IsEnabled() bool
}
```

An interface has every method of the interfaces it embeds.  In the
generic AST the embed is a **Spread** expression field
(`G.F (ExprStmt (Call (IdSpecial Spread, [Arg name])))`), **not** a
`DefStmt FuncDef`, so the field list of `Client` declares only `Name`
and `IsEnabled`.

`class_body_extra_parents` (`go_class_body_extra_parents`) returns each
embedded type as a parent appended to the resolution order, and
`walks_inheritance` makes `Mro.inherit_into_type_state` attach the
methods of `Authenticator` to `Client`
(`tests/interfile/go/interface_embedding/`,
`tests/interfile/go/embedding_chain_four_levels/`).

### Methods of classes with one simple identifier in several packages

Several packages of a Go project can each define
`type Service struct { ... }` with a `Get(...)` method.  For
`s.Get(...)` where the class of `s` is known,
`Callee_resolution.identify_callee` looks the method up by class and
method identifier and narrows the candidates in `resolve_class_method`:

1. **Package qualifier:** where the type is qualified (`*foo.Service`),
   keep the candidates of that package (`narrow_by_package_qualifier`).
2. **Same file:** prefer the candidates defined in the caller's file.
3. **Same directory:** when several remain, prefer, for Go, those in
   the caller's directory, which is one package.
4. **Interface:** a candidate that is the declaration of an interface
   method (`FBDecl`) is the callee.
5. **Arity:** otherwise `pick_by_arity` chooses by parameter count.

The narrowing applies where the receiver is an identifier with a known
type and where the type of the receiver is inferred from the
expression.

## Ruby

### `attr_reader` / `attr_writer` / `attr_accessor`

```ruby
class User
  attr_reader :name, :email
end
```

These macros declare accessor methods at runtime.  `class_body_synth_methods`
walks each class body looking for those calls and returns
`[("name", def_tok); ("email", def_tok)]` where `def_tok` points at
the symbol literal so call-site resolution to it matches the
indexer's output.

### `include` / `extend` / `prepend`

```ruby
class Foo
  include Helpers::Strings
  extend Concerns::Cacheable
end
```

These add `Helpers::Strings`'s and `Concerns::Cacheable`'s methods to
`Foo`'s MRO.  `class_body_extra_parents` returns the dotted-name
paths which get appended to `ci_parent_paths` so the existing MRO
walk inherits the methods.

### Blocks are *not* call-graph vertices

Ruby's `array.each do |x| ... end` parses as a block, which is a
function in the generic AST.  `include_anonymous_funcs = false` keeps
blocks out of the vertices of the call graph.

The block not being a vertex does **not** mean the *call* is
ignored: `f(real_args) do |x| ... end` (Ruby/Crystal/Scala) parses
as `Call(Call(f, real_args), [block])`, and `identify_callee`
resolves the edge against the inner call so `f` is still linked,
with the block collected as a HOF callback (see
[§ 6 B9](06-subtleties.md)).

### RSpec spec files

projidx resolves a method call through the class of its receiver, not
by the method identifier alone.  A test stub's `def create` is
therefore not linked to every `create` of the application code, and
the contents of a `shared_examples` block are not linked to every spec
that includes it.

## TypeScript / JavaScript

### `tsconfig.json` excludes and paths

`tsconfig.json` files declare `exclude` arrays.
`discover_project` (`Ts_modules.discover`) walks the directory tree from
the project root (skipping `node_modules`, `.git`, `.yarn`, `dist`,
`build`, `.cache`, with a depth cap), in each directory taking
`tsconfig.build.json` if present, else `tsconfig.json`, and
accumulates their `exclude` patterns.  Each pattern is made
relative to the project root (`Ts_modules.normalize_pattern`).  The `paths` of the configuration files at the project
root map specifiers to files
(`tests/interfile/typescript/tsconfig_paths_binds_file/`).

### Exports and imports

```typescript
// foo.ts
export default class Handler { ... }
export const helper = () => {};

// bar.ts
import Handler from "./foo";
import { helper } from "./foo";

new Handler();
helper();
```

`unqualified_scope` is `Per_module`, and `Scope_module` builds the
scopes.  `Scope_module.build_exports` computes the exports of every
module as a fixpoint over the export rows of each file
(`Scope_module.rows_of_file`):

- `export` of a local identifier exports that identifier; a default
  export is the export `default`.
- `module.exports = f` exports `f` as `default`, `module.exports = { a,
  b }` exports each field, and `exports.f = g` exports `g` as `f`
  (`tests/interfile/javascript/module_exports_default_fn/`,
  `tests/interfile/javascript/cjs_named_exports/`).
- A re-export of one identifier exports the export of its target
  (`tests/interfile/typescript/reexport_named_binds_target/`), `export *
  as N from "./y"` exports the module `./y` as `N`, and `export * from
  "./y"` exports every export of `./y` except `default` that the module
  does not already export (`tests/interfile/typescript/reexport_star_binds_target/`).

`Scope_module.build` binds each import of a file to the export it refers
to.  `import { X } from "./y"` binds `X` to the export `X` of `./y`, and
`import X from "./y"` is an import of the export `default`
(`tests/interfile/typescript/default_import_binds_class/`).  An import
of a whole module (`import * as N from "./y"`, `const N =
require("./y")`) makes `N` an alias of the module, so `N.f` resolves
among the exports of `./y`, and binds `N` to the `default` export of
the module when it has one
(`tests/interfile/typescript/namespace_import_member/`,
`tests/interfile/javascript/require_binds_module/`).  A type-only import
and a re-export bind nothing in the importing file
(`tests/interfile/typescript/type_only_import_binds_nothing/`,
`tests/interfile/typescript/export_star_does_not_bind_parent/`).  A file
without imports or exports is a script: its own definitions bind in
every script.

`specifier_candidates` (`candidates_of_base`) gives the files that a
specifier may refer to: the literal path, the source extensions, and
`index` files under the path
(`tests/interfile/typescript/index_specifier_binds_directory/`).

A field of a class whose initialiser is an identifier bound at module
level to a function, `class C { handler = p }`, makes `C.handler(...)`
resolve to that function (`Scope_module.class_field_aliases`,
`tests/interfile/typescript/class_alias_via_imports/`).

### Lambda VarDefs

```javascript
const callback = () => doStuff();
list.forEach(callback);
```

Lambda VarDefs (`const X = () => ...`) are call targets and belong to
the per-file visibility set, so call sites in the same file resolve
them by identifier.  The Phase 1 vertex collection and the
per-file `funcs_by_name` visibility filter both treat `Func_def`,
`Class_def`, and `Var_def` (with a lambda RHS) uniformly via the
`Walker.Observation.t` variant.

## Rust

### `impl Foo { fn bar() }`

Rust's parser emits this as

```
DefStmt(ent, OtherDef("Impl", [T(Foo); Ss(stmts)]))
```

`Visit_function_defs.class_scope_of_definition` rewrites every
`OtherDef("Impl", ...)` into

```
DefStmt({name=Foo}, ClassDef{cbody=stmts; ...})
```

with synthetic class metadata: the first type of the `impl` is the
class, the remaining types are its `cextends`, and implements and
params are empty.  `Visit_function_defs` and `Graph_from_AST` apply
the rewrite to every definition they visit, and the Rust entry sets it
as `class_def_reshape`, which the symbol collector (`Symbols`) applies,
so all three bind the methods to the type of the `impl`.  The AST is not
rewritten in place.  Go's `class_def_reshape` (`go_class_def_reshape`)
applies in the symbol collector only.

## PHP

PHP sets `walks_inheritance = true` (the `extends` chain is walked
like Java's), `package_directive_is_namespace = true` (`namespace
App\Svc;` parses to `Package`/`PackageEnd` directives that scope qns
rather than identifying the file's module), and
`unqualified_scope` is `Per_namespace` (`Scope_php` binds the
unqualified identifiers of a file by namespace).  Two field-typing
hooks: `strip_field_sigil` drops the
`$` so `$this->x` and promoted ctor params share one field
namespace, and `ctor_param_promotion` registers every typed
constructor param as a candidate field (PHP 8 promotes them; the
parser drops the visibility modifier).  Field types otherwise come
from the shared self-assignment pass.

### `Class::staticMethod()` vs `Class->instanceMethod()`

PHP's static-call syntax `Class::staticMethod(...)` resolves to a
static method of `Class`, not to an instance method of the same
identifier in an unrelated class.  The obj.method() site in
`identify_callee` therefore returns `None` (rather than a bare-name
project-wide fallback) when the imported-name lookup fails:
returning a candidate without a class-context check would alias
`Class::staticMethod(...)` to methods on unrelated classes that
share the bare name.  The static resolution path is `import name →
class qualifier → exact static method`, with no name-only fallback.

## Java / Kotlin

### `package x.y.z;` and `Per_package` scope

Java and Kotlin declare a file's package explicitly:

```java
// src/main/java/com/example/Helper.java
package com.example;
class Helper { void process(String s) { ... } }

// src/wherever/Main.java
package com.example;
class Main {
  public static void main(String[] args) {
    new Helper().process("...");   // no `import` needed
  }
}
```

The files of one package are visible to each other without any
`import`, wherever they are on disk: the directory and the package
declaration are independent.

Two fields handle this:

- `module_path_from_ast` is `extract_package_decl`, which looks in the
  top level for a `G.Package(_, parts)` directive and returns the
  dotted path.  When it returns `Some "com.example"`, that path
  overrides the file-path-derived module path.
- `unqualified_scope` is `Per_package`, which keys the cross-file
  visibility index on `fi_module_path` instead of the file's parent
  directory.
  Two files declaring `package com.example;` end up under the same
  scope key, whatever the directories that hold them.

### Inheritance walk

Java/Kotlin classes can `extends Animal` / inherit `Animal`'s
methods.  `walks_inheritance = true` triggers `Mro.inherit_into_type_state`
which attaches the parent's methods to the subclass.

The MRO walk derives the child's class identity from its own
methods' `fn_id`s when it has any; for an empty subclass like
`class Dog extends Animal {}` it synthesises the IL name from the
class's simple name, so empty subclasses inherit their parents'
methods like any other subclass.

### Abstract methods have no body

An abstract or interface method lowers to `FBNothing`: dispatch merges
its implementations' signatures into the declaration for taint, which
gives it a signature, not a body.  So a pattern with a body,
`void $F(...) { ... }`, matches the concrete methods only, and a
declaration pattern, `void $F(...);`, matches the abstract and the
interface ones.  The pattern tests
`tests/patterns/java/abstract_method_has_no_body.java` and
`tests/patterns/java/abstract_method_declaration.java` cover both
forms.

### Lambdas

Stream chains (`.filter(x -> ...)`) and lambdas in callbacks are not
vertices of the call graph: as for TypeScript,
`include_anonymous_funcs = false`.

### `import` resolution

The Java/Kotlin tree-sitter parsers emit
`G.ImportFrom(_, DottedName ["com"; "example"], [("Foo", None)])`
for `import com.example.Foo;`, which `collect_imports` records as
for any other language.  Only the package declaration has a
Java-specific hook.

## C#

### `namespace X.Y;` (file-scoped) vs `namespace X.Y { ... }` (block-scoped)

A file-scoped namespace applies to the rest of the file:

```csharp
namespace App.Core;

class Service { ... }
```

The C# parser emits `G.Package(_, ["App"; "Core"])` for it, which
`extract_package_decl` reads, as for Java and Kotlin.

Block-scoped namespaces (`namespace X { ... }`) parse as
`G.DefStmt(_, G.ModuleDef _)`, not `G.Package`.  A block-scoped
namespace is **not** the file's module path; the file has the
file-path-derived module.  A file with several namespaces also has the
file path as its key.

### `using` directives

`using System.IO;` parses as `G.ImportAll(_, G.DottedName ["System"; "IO"], _)`
(wildcard import).  `using Foo = System.Text;` parses as
`G.ImportAs(_, _, Some(Foo, _))`.  `collect_imports` records both
shapes as for any other language.  `using static System.Math;` also
parses as `ImportAll`: the parser does not distinguish it from a
non-static `using`.

### Inheritance

`class Dog : Animal, IAnimal`: C# uses `:` for both base class and
interfaces, all in one list.  `walks_inheritance = true` handles
the base class lookup; interface implementation is not structurally
distinguished and is not walked separately.

## C++

### `namespace X { ... }` and the module path

```cpp
namespace app {
  namespace core {
    void process(std::string s) { ... }
  }
}
```

The C++ parser converts `namespace X { ... }` into

```
G.Package(_, [X])   // followed by the body, then G.PackageEnd
```

`namespace_decl_or_global` (through `extract_package_decl`) takes
the first, **outermost** namespace as the file's module path.  Nested
namespaces (`namespace core` inside `namespace app`) do not contribute:
the file's module is `app`.  A file with several top-level namespaces
takes the first.  A file without a
namespace has the global namespace, the empty module path, not the
file-path-derived module.  Classes are qualified by the namespace open
at their definition (`package_directive_is_namespace`), and
unqualified identifiers bind per translation unit, as in C (§ C).

### `#include` resolution

`#include "handler.h"` parses as
`G.ImportAll(_, G.FileName "handler.h", _)`, a wildcard import.  The
raw specifier is `"handler.h"` (with extension), but the module path of
the corresponding source file is `handler` (path-derived, with the
extension stripped by `Fpath.rem_ext`).

`normalize_import_specifier = strip_c_header_ext` strips the
header suffix (`.h`/`.hpp`/`.hh`/`.hxx`) before it becomes a module
key.  System includes (`#include <stdlib.h>`) get the same
treatment.

### Forward declarations

```cpp
// Main.cpp
class Animal { ... };  // forward declaration
int main() { Animal a; ... }
```

A forward declaration of `Animal` in `Main.cpp` registers a
**second** `class_info` for `Animal` (alongside the real one in
`Animal.cpp`) with **no methods**.  When `Mro.inherit_into_type_state`
resolves the parent of `Dog`, the lookup can return the `Animal`
without methods, and then `Dog` inherits no methods.  An
`#include "Animal.h"` in place of the forward declaration avoids the
second `class_info`.

## C

### Linkage and `Per_translation_unit` scope

C has no `package`/`namespace` declaration.  A function with external
linkage defined in one `.c` file is callable from another translation
unit that declares it, usually through a shared header.
`unqualified_scope` is `Per_translation_unit`: `Scope_c_family` binds an
unqualified identifier to the definitions of the file and, through
`Include_map`, to every function without the `static` attribute that a
file in the file's `#include` closure declares, when the defining
file is that file or reaches it through its own includes; a declaring
file is one that another file includes.

### `#include "handler.h"`

A C `#include` parses as in C++, to `G.ImportAll(_, G.FileName, _)`,
and the C entry uses the same `strip_c_header_ext` to drop the header
suffix.

### No inheritance walk

C has no classes.  `walks_inheritance = false`.

## Clojure

### `(ns x.y.z)` declaration

Clojure declares the file's namespace as the first form:

```clojure
(ns app.handlers
  (:require [clojure.string :as str]))

(defn handle [msg]
  (println msg))
```

The Clojure tree-sitter parser wraps the entire `ns` form into a
single `G.OtherDirective(("NsDirective", _), exprs)`.  It emits
neither `G.Package` nor import directives for the `:require` clauses,
so projidx reads the NsDirective itself.

`extract_clojure_ns_decl` looks in the top level for the NsDirective
and returns its first form, the namespace.  It returns
`Some "app.handlers"` for the example above.

### `:as` aliased requires

```clojure
(ns app.main
  (:require [app.handlers :as h]))

(h/handle x)
```

`collect_imports` recognises the
`OtherDirective("NsDirective", ...)` shape, finds the
`(:require [m :as h])` sub-form, and:

- adds `(h, app.handlers)` to `fi_imports` as an alias entry, and
  `Pipeline.build_alias_to_module_qn` makes `h` resolve to the
  `app.handlers` module qn;
- adds `("*", app.handlers)`, a wildcard import, which binds the
  definitions of `app.handlers` in the importing file.

At the call site, `(h/handle x)` parses as
`G.Call(G.N(IdQualified{name_middle=["h"]; name_last="handle"}), ...)`.
`identify_callee`'s `IdQualified` branch consults the alias index:
when `name_middle` is a single segment that resolves via
`alias_to_module_qn`, it looks up `name_last` in
`funcs_in_module` of the target module.

### `:refer [name1 name2]`

```clojure
(:require [app.handlers :refer [handle greet]])
```

For each name in the refer vector, `collect_imports` adds
`(name, app.handlers.name)` to `fi_imports` (same shape as
Python's `from M import name`).  At the call site, `(handle x)` is
a bare `Id` and resolves via the standard
`alias_to_module_qn`-based imported-name fallback in
`identify_callee`.

### No Java-style inheritance walk

`defprotocol`/`defrecord` are protocol-based polymorphism, not
class hierarchies.  `walks_inheritance = false`.  Inline `(fn [x]
...)` and `#(...)` anonymous functions are vertices
(`include_anonymous_funcs = true`).

### `Per_file` scope, not `Per_package`

A Clojure file holds **one namespace**, and refers to another
namespace through `:require`.  `Per_package` would make the files of
sibling namespaces visible without `:require`, so Clojure uses
`Per_file`.

## Solidity

Solidity sets `walks_inheritance = true`, `method_member_scope =
Members_in_class_scope`, `bases_listed_most_base_first = true`,
`specifiers_name_files = true`, `path_import_binds_all_names = true` and
`wildcard_import_omits_underscore_names = false`.  Three `Lang_config`
facts hold for Solidity: `overloads_by_type`, `inherited_overloads_merge`
and `super_follows_most_derived_order`.

### Path imports

`import "./Base.sol";` without an alias brings every global symbol of
the file into scope, including those whose identifier begins with an
underscore.  The front end emits `ImportAs (FileName "./Base.sol",
None)`.  `specifiers_name_files` resolves the specifier against the
files of the project, and `path_import_binds_all_names` makes
`collect_imports` record the import as a wildcard import of that file.
Because `wildcard_import_omits_underscore_names` is `false`,
`Reexports.star_exported` keeps a function such as `_leak` in the
binding.  Fixtures: `tests/interfile/solidity/path_import_binds_all_names/`
(a control file with the same definitions that no file imports stays
clean) and `tests/interfile/solidity/path_import_binds_underscore_name/`.

A wildcard import binds the attributes of the imported module, which
`Func_index.build_attributes_by_module` computes: the module's own
definitions, the identifiers that its single-symbol imports bind, and,
through a fixpoint over the wildcard imports of every file, the
identifiers that its own wildcard imports bind.  So `import
"./Middle.sol";` also binds the functions of a file that `Middle.sol`
imports with a path import.  An identifier that a module binds by a
definition or a single-symbol import keeps that binding; a wildcard
import does not replace it.  The imports enter the attributes of a
module only where `unqualified_scope` is `Per_file`, `Per_crate` or
`Per_directory` (`imports_bind_attributes`); Solidity, Bash and Circom
use `Per_file`.

Limits:

- A specifier without a leading dot is resolved only through the module
  paths that `discover_project` returns, and the Solidity entry returns
  none.  Import remappings are not read, so `import
  "@openzeppelin/contracts/...";` binds nothing.

### Linearisation

`contract D is A, B` lists the most base-like contract first, and
Solidity linearises the bases with C3 from the right: `B` comes before
`A` in the order of `D`.  `bases_listed_most_base_first` makes `Symbols`
reverse the extends list before `Mro` runs its C3 merge
(`tests/interfile/solidity/linearisation_super/`).

### Inherited members

A bare identifier in a function of a contract refers to any function
along the contract's linearisation.  With `Members_in_class_scope`,
`Scope_package.method_scope_bindings` binds, in the scope of the
contract, the functions of the first class along the resolution order
that defines the identifier (`tests/interfile/solidity/inherited_member_call/`).

A contract inherits the overloads of a function that it does not
override, and an override has the parameter types of the function it
overrides.  Where `Lang_config.inherited_overloads_merge` holds,
`Scope_package.members_of_order` and `Func_lookup.find_along_order` add
the functions of a farther class to the nearer ones unless a nearer
function has the same arity (`Func_lookup.with_inherited_overloads`).
`tests/interfile/solidity/inherited_overload_of_other_arity/` resolves
`g(source(), 1)` in `D` to the two-parameter `g` of `Base`, and
`tests/interfile/solidity/override_hides_inherited_function/` keeps the
overridden `Base.g` out.

### Free-function overloads

Several free functions of one identifier in a file are overloads.
Because `Lang_config.overloads_by_type` holds for Solidity, the per-file
scope groups a file's own definitions of one identifier into one
binding (`Scope_binding.overload_sets`), and `pick_by_arity` chooses
among them by the arity of the call
(`tests/interfile/solidity/free_function_overloads_in_one_file/`).
Among several of the call's arity in one scope,
`Callee_resolution.overload_representative` returns the earliest by
position, and interfile dispatch widens its signature to the union over
the group (`Structural_dispatch.emit_overload_edges`).

Limits:

- Overloads of one arity are not told apart by their parameter types.
- Along a resolution order, a nearer function hides an inherited one of
  the same arity, whatever their parameter types
  (`Func_lookup.with_inherited_overloads`).

### `super`

`super.f()` in contract `B2` calls the next `f` after `B2` in the
linearisation of the most derived contract of the object, not in the
linearisation of `B2`.  The front end lowers the identifier `super` to
`IdSpecial Super`.  Where `Lang_config.super_follows_most_derived_order`
holds, `Project_index` builds `derived_orders`
(`Func_lookup.derived_orders_of`: for each class, the resolution orders
of the classes that no other class inherits from and that contain it),
and `Callee_resolution.orders_after_class` looks up `f` after `B2` in
each of those orders and merges the candidates.
`tests/interfile/solidity/super_follows_most_derived_order/` (`contract
Final is B1, B2`) resolves `super.f` in `B2` to `B1.f`;
`tests/interfile/solidity/super_outside_most_derived_order/` (`contract
Final is B2`) stays clean.

Limit: a call site has one callee.  When the orders of the most derived
classes that contain the caller's class give different functions of the
call's arity, `pick_by_arity` returns none, and the call resolves to no
function.

## Dart

Dart sets `library_parts_of_ast = dart_library_parts`,
`walks_inheritance = true`, `method_member_scope =
Inherited_members_after_lexical_scope`, `mixin_extends_lists_constraints
= true`, `superclass_is_never_a_mixin = true`,
`class_body_extra_parents = mixins_last_applied_first`,
`superclass_position = Superclass_after_mixins`, a `name_is_exported`
that rejects a leading underscore, `specifiers_name_files`,
`specifiers_are_uris` and `discover_project = dart_discover`.  The
`Lang_config` fact `super_follows_most_derived_order` holds for Dart.

### Mixins

A Dart mixin is a class that another class applies with `with`.  The
front end emits `mixin M on B implements I { ... }` as a `ClassDef` of
kind `Trait`, with its `on` types in `cextends` and its `implements`
types in `cimplements`.  Mixins apply in order, so the last one is the
nearest: `class C extends A with M1, M2` has the order `C, M2, M1, A`.
`mixins_last_applied_first` reverses the `with` list, and
`Superclass_after_mixins` puts the superclass after it
(`tests/interfile/dart/mixin_linearisation/`).

### `on` types

`mixin M on B` restricts `M` to classes that are subtypes of `B`; `B`
is not a parent that `M` adds to a class.  With
`mixin_extends_lists_constraints`, `Symbols` gives the extends clause of
a `Trait` the position `Constraint`.  `Mro` leaves a `Constraint` parent
out of every linearisation and appends its order to the mixin's own
resolution order only.  `tests/interfile/dart/mixin_on_type_not_ahead_of_superclass/`
resolves `f` in `class C extends A with M` to `A.f`, not to the `f` of
the `on` type `B` that `A` overrides;
`tests/interfile/dart/mixin_on_type_after_superclass/` resolves it to
`A.f` where `B` is abstract.

### `super` in a mixin

`super.f()` in a mixin calls the `f` of the superclass of the mixin
application: the class after the mixin in the linearisation of the
class that applies it, not the `on` type.  `super_follows_most_derived_order`
holds for Dart, so `super` follows the same `derived_orders` as in
Solidity.  In a class that no class applies as a mixin, the classes after
it are the same in every order that contains it, so the rule changes
nothing there.
`tests/interfile/dart/mixin_super_follows_applying_class/` resolves
`super.f` in `mixin M on B` applied by `class C extends A with M` to
`A.f`; `tests/interfile/dart/mixin_super_skips_overridden_on_type/`
keeps the `f` of `B`, which `A` overrides, clean.

Limit: as in Solidity, a call site has one callee.  When the orders of
the most derived classes that contain the mixin give different
functions after it, `super.f()` in the mixin resolves to no function.

### `implements`

`implements` takes the interface of a class, not its implementation.
`Symbols` builds the parent paths of a class from `cextends` and
`class_body_extra_parents` only, so a class in `cimplements` never
enters a resolution order and no call resolves to its members through
the implementing class.

### Bare identifiers in methods

In a Dart method a bare identifier is looked up in the lexical scopes
first: the method, the class body, the library and its imports.  Only
when none of them binds it does it refer to an inherited instance
member.  With `Inherited_members_after_lexical_scope`,
`Scope_package.method_scope_bindings` returns the class's own members
with the lexical bindings and the inherited members separately, and
`Pipeline` puts the inherited members behind the whole file scope
(`Func_lookup.scope_table_shadowing ~back`).  An inherited member is
left out when it is `static`, or when it is defined in another library
and its identifier begins with an underscore (private to its library).
A library is a file and the files that its `part` directives add to it
(`library_parts_of_ast`, `Scope_binding.same_library`).
Fixtures: `tests/interfile/dart/implicit_member_call/`,
`tests/interfile/dart/private_member_of_part_file_inherited/`,
`tests/interfile/dart/library_function_ahead_of_inherited_member/`,
`tests/interfile/dart/inherited_static_member_not_in_scope/` and
`tests/interfile/dart/private_member_of_other_library_not_inherited/`.

### Imports

`import 'helpers.dart';` is a wildcard import that leaves out the
top-level definitions whose identifier begins with an underscore
(`tests/interfile/dart/private_function_of_imported_library/`).
`import 'a.dart' as a;` binds the prefix `a`
(`tests/interfile/dart/prefixed_import/`).  A specifier without a scheme
is relative to the importing file.  `dart_discover` reads the `name:`
line of `pubspec.yaml` and maps `package:<name>/*` to `lib/*`
(`tests/interfile/dart/package_specifier/`); a `dart:` URI or a
`package:` URI of another package binds nothing.

## Bash

Bash sets `specifiers_name_files = true`, `bare_specifiers_relative_first
= true`, `file_imports_of_expr = shell_source_imports` and
`wildcard_import_omits_underscore_names = false`.  Its unqualified
scope is `Per_file`.

### A command is a call

Bash looks the first word of a simple command up among functions,
builtins and programs, never among variables.  The front end emits the
simple command `f a b` as `Call (N "!sh_cmd!", ["f"; a; b])`.
`AST_generic_helpers.call_of_shell_command` turns a command whose first
word is a literal into a call of that word with one argument, the list
of the remaining words; `AST_to_IL` and `Graph_from_AST` both apply it.
The callee identifier is resolved to its own site, so it never refers to
a variable with the same identifier: in
`tests/interfile/bash/command_namespace/`, `$(date +%s)` runs the
command `date` although a variable `date` holds tainted data.

### Positional parameters

A shell function declares no parameters.  A call sets its positional
parameters, which the body reads as `$1`, `$2`, ..., and all at once as
`"$@"` or `"$*"`; `shift` and `set -- ...` renumber them.  The front end
gives each function of a target one parameter, a pattern over the list
of the call's words (`Bash_to_generic.positional_parameters_pattern`);
a function of a pattern has the parameter `...` instead.

The positional parameters that the body reads come from the variables
it expands (`Bash_to_generic.use_of_command`), outside the functions it
defines:

- Each of `$1` to `$9` binds its element of the list, up to the highest
  position the body reads (`tests/interfile/bash/positional_parameter/`).
  An expansion with an operator, such as `${1:-default}`, reads its
  variable (`tests/interfile/bash/default_value_argument/`).
- `$@`, `$*` and every other variable whose identifier begins with a
  digit and has more than one character, such as the `10` of `${10}`
  and of `$10`, bind the whole list (`Bash_to_generic.is_positional_name`,
  `tests/interfile/bash/all_positional/`).  A `for` loop without `in`
  iterates over `"$@"`, so it reads the whole list
  (`tests/interfile/bash/for_without_in_argument/`).
- A literal `shift`, or `shift N` with `N` a single digit, at the top
  level of the body, before any read, offsets the elements
  (`tests/interfile/bash/shift/`).  When the body then reads no
  single-digit position, `$@` and `$*` bind the list without its first
  `N` elements, through a rest pattern: a trailing `PatConstructor`
  whose identifier is `...`, which `AST_to_IL` and
  `Dataflow_tainting.pattern_leaves_with_offsets` read as a rest in
  every language (`tests/interfile/bash/shift_all_positional/`).
- A `shift` after a read or inside a loop, and a `set` that replaces
  the positional parameters, make the positions unknown statically;
  every positional parameter then binds the whole list
  (`tests/interfile/bash/shift_in_loop/`,
  `tests/interfile/bash/set_positional/`).
- `set` replaces the positional parameters when an argument follows its
  options, when `--` ends them, or when `-` ends them and an argument
  follows (`Bash_to_generic.sets_positional_parameters`).  A word of
  single-letter options such as `-eu` or `+x` is one option cluster, and
  each `o` in it takes the next word as an option argument.  So `set
  -euo pipefail`, `set -e -o pipefail` and `set -` renumber nothing
  (`tests/interfile/bash/set_option_cluster/`,
  `tests/interfile/bash/set_positional/`).
- A function defined inside another has positional parameters of its
  own (`tests/interfile/bash/nested_function/`).
- `f "$@"` passes the positional parameters to `f` at their positions:
  in `tests/interfile/bash/forward_all_positional/`, `second "$@"` does
  not carry the first argument of the caller into `$2`.

The shell commands of a Dockerfile `RUN` instruction go through the
Bash converter (`Dockerfile_to_generic`), so a function defined there
binds its positional parameters in the same way
(`tests/rules/cross_function_tainting/dockerfile_function_parameter.dockerfile`).

An expansion with a subscript, such as `${a[i]}` or `${words[*]}`, is a
todo node of the generic AST (`Bash_to_generic.todo_expr`); the
positional parameters that its subscript and its operands expand are
read by the body all the same.

### A function returns its output

`$(f)` reads what `f` writes.  The front end gives every shell function
of a target the attribute `command_output_attribute`; a function of a
pattern carries no such attribute.  For a function with the attribute
(`returns_command_output`), `AST_to_IL` starts a fresh `output` variable
with the arguments of the function, appends the value of each expression
statement other than an assignment (`command_output_stmt`), and returns
that variable at the end of the body.  A condition is a command too:
`condition_output` appends the value of the condition of `if`, `elif`,
`while` and `until` to the output.

The output carries every argument because Bash variables have dynamic
scope.  A function that the body calls reads and assigns the variables
of its caller, and a child process reads the exported ones, so an
argument reaches the output through a variable that the body does not
expand.  The arguments also cover the redirections, which the
conversion to the generic AST drops (`Bash_to_generic.cmd_redir`): a
here-string and a process substitution on the input.  In
`tests/interfile/bash/function_output_carries_arguments/`, the output
of a function carries its argument through a here-string
(`sed ... <<< "$1"`), through a process substitution on the input of a
loop, through a variable that a called function assigns, and through
the condition of an `if` whose output is piped; the output of a call
with a constant argument carries no taint.

Limits:

- The output of a function carries the taint of every argument, also
  of an argument that no command of the body writes.
- A position beyond the words of the call carries the taint of the
  whole word list: where the list has no element at the position,
  `Taint_shape.find_in_shape_w_carry` returns the taint of the list.
- A command whose output is redirected to a file still adds that output
  to the output of the function: the conversion drops file redirections
  (`Bash_to_generic.cmd_redir`).

### `source` and `.`

`source f` and `. f` run `f` in the current shell, so its functions are
defined in the caller.  `shell_source_imports` recognises either command
with a single literal argument, also as an operand of `&&` or `||`, and
`collect_imports` records it as a wildcard import of that file.  With
`bare_specifiers_relative_first`, a specifier is resolved from the
directory of the script first: `source lib.sh`
(`tests/interfile/bash/source_bare/`), `source ./lib.sh`
(`tests/interfile/bash/source_file/`) and `. ./lib.sh`
(`tests/interfile/bash/dot_file/`).  `[ -f ./lib.sh ] && . ./lib.sh`
(`tests/interfile/bash/source_and/`) and `. ./lib.sh || exit 1`
(`tests/interfile/bash/source_or/`) are resolved too.  `! . ./lib.sh`
runs `. ./lib.sh` and inverts only its exit status, so it imports the
file as well (`tests/interfile/bash/source_negated/`).  Functions whose
identifier begins with an underscore are bound
(`tests/interfile/bash/source_underscore_function/`).  A function of a
file that is not sourced is not in scope
(`tests/interfile/bash/not_sourced/`).  A wildcard import also binds
the functions of the files that the sourced file sources in turn
(`Func_index.build_attributes_by_module`, see Solidity).

Limits:

- A `source` whose argument is not a single literal word, such as
  `. "$dir/lib.sh"`, or that passes arguments after the file, is not
  resolved (`tests/interfile/bash/source_computed/`).
- `collect_imports` reads every statement of the file
  (`Walker.fold_stmts_in_program`), so a `source` inside a subshell,
  `( . ./lib.sh )`, is recorded as an import of the file, although the
  functions that it defines exist only in the subshell.

## Cairo

Cairo 1 has Rust's modules, `use` paths and `impl` blocks, and a Scarb
package is its crate.  The Cairo entry extends the Rust entry with
`crate_manifest = Some "Scarb.toml"`, `crate_root_stems = ["lib"]`,
`unaliased_import_binds = Last_segment_binds` and
`wildcard_import_omits_underscore_names = false`.  `Rust_crates` reads the
`[package] name` of each `Scarb.toml` and puts it at the head of the
module path of the files under it.

- Only `src/lib.cairo` is the root module of the crate; `src/main.cairo`
  is the module `main`.  `tests/interfile/cairo/main_file_is_a_module/`
  resolves `use crate::main::helper;`, and
  `tests/interfile/cairo/main_file_is_not_the_crate_root/` keeps `use
  crate::helper;` from reaching `src/main.cairo`.
- `use crate::sinks::relay;` binds `relay`, the last segment
  (`tests/interfile/cairo/crate_use/`); `use crate::sinks::relay as
  forward;` binds `forward` (`tests/interfile/cairo/use_alias/`).
- The functions of one `mod` call each other by bare identifier
  (`tests/interfile/cairo/module_member_call/`).

## Move

Move on Aptos and Move on Sui share the `module_scoped` entry.  A module
(`module 0x1::m { ... }`) is a namespace whose functions call each other
by bare identifier, and `use 0x1::m;` binds `m`.  The entry is the
Elixir entry with `wildcard_import_omits_underscore_names = false`: it
sets `unqualified_scope = Per_namespace`, `module_definition_is_namespace
= true` (a module definition opens a namespace scope) and
`unaliased_import_binds = Last_segment_binds`.
`tests/interfile/move_on_aptos/module_use/` and
`tests/interfile/move_on_sui/module_use/` resolve `sinks::relay(...)`
after `use 0x1::sinks;` and `use pkg::sinks;`.

## Circom

`include "lib.circom";` brings every template and function of the file
into scope.  The front end emits `ImportAll (FileName "lib.circom")`.
The Circom entry sets `specifiers_name_files`.  It also sets
`bare_specifiers_relative_first`, so a specifier is looked up relative
to the including file first, and `wildcard_import_omits_underscore_names
= false`, so definitions whose identifier begins with an underscore are
bound (`tests/interfile/circom/include_file/`,
`tests/interfile/circom/include_underscore_function/`).

An include also binds the definitions of the files that the included
file includes in turn (`Func_index.build_attributes_by_module`, see
Solidity).

Limit: the entry supplies no module paths, so a file found only
through a library directory of the compiler (`-l`) is not resolved.

## Hack

`for_lang` maps Hack to the PHP entry, so Hack has PHP's namespace scope
(`Per_namespace`), its field hooks (`strip_field_sigil`,
`ctor_param_promotion`), and `unaliased_import_binds =
Last_segment_binds`.

### `use`

Every `use` clause is an `ImportAs` of its whole path, with the alias of
the clause when it has one: `use function A\B\f;` imports `A\B\f`.  The
kind of the clause, or else the kind written after `use` for a list or
a group, gives the attributes of the directive
(`Parse_hack_tree_sitter.use_kind_attrs`): `use function` carries
`Callable` and `use const` carries `Const`, as in the PHP front end.
`use type`, `use namespace` and a plain `use` carry no attribute, as a
plain `use` imports both the namespace and the type.  `collect_imports`
(`Imports.ml`) binds the last segment of the path, as a function for
`Callable` (`Binds_function`), as a constant for `Const`
(`Binds_constant`), and as any definition otherwise (`Binds_any`).

- `use function App\Util\{quiet, relay};` binds both functions, and
  `use type App\Util\Box;` binds the class
  (`tests/interfile/hack/namespace_use/`).
- In `use function App\Util\relay, const App\Util\quiet;` the second
  clause carries its own kind, so `quiet` binds as a constant and the
  call `quiet(...)` does not resolve to the function
  (`tests/interfile/hack/list_clause_kind_binds_constant/`).
- `tests/interfile/hack/plain_use_binds_class/` resolves `Store` after
  `use App\Util\Store;` to `App\Util\Store`, not to `App\Other\Store`.

### Member access

The front end emits a member access `$o->m` as `DotAccess (o, FDynamic
m)`, in targets and in patterns, as it emits `$o->$m` and `$o->{e}`
(`selection_expression`).  A Hack variable begins with `$`, so
`AST_to_IL.member_of_dynamic_field` lowers a member that is an
identifier without `$` to a field offset (`Dot`), and
`Graph_from_AST.callee_with_member_identifier` gives a callee
`$o->m(...)` the member `FN m` before callee resolution, so the call
resolves as a method call (`$box->put(...)` in
`tests/interfile/hack/namespace_use/`).

## QL

`import M` without an alias brings every top-level declaration of `M`
into scope;
`import M as N` binds `N` to `M`.  The QL entry sets
`unaliased_import_binds = Every_name_binds`, which makes
`collect_imports` record an unaliased import as a wildcard import of
`M`.  `tests/interfile/ql/module_import/` resolves `relay` in `main.ql`
after `import sinks` to the predicate of `sinks.qll`.

## R

The files of an R package share one namespace, and a function is a
lambda assigned to an identifier.  The R entry sets `unqualified_scope = Per_project`: `Scope_project.build`
binds the top-level definitions of every file in every file, so a call
resolves without `source` (`tests/interfile/r/shared_namespace/`).  It
also sets `assigned_function_defines_identifier = true`, so
`f <- function(x) ...` defines `f` in the scope of the assignment
(`tests/interfile/r/assigned_function_shadows_other_file/`).

## OCaml

The OCaml entry sets `rewrite_module_path` and
`wildcard_import_omits_underscore_names = false`.  A compilation unit is
a module whose identifier is the basename of its file, capitalised
(`Lib` for `dir/lib.ml`).  The directory is not part of the module path,
and the unqualified scope is `Per_file`.
`tests/interfile/ocaml/cross_file_call/` marks the sink reached through
`Lib.leak (source ())` as `todoruleid`: the flow is not reported.

## Common to all languages: HOF callback chains

Higher-order function patterns appear in every language:
TypeScript's `arr.map(cb)`, Python's `decorator`, Go's
`http.HandleFunc(path, handler)`, Ruby's `delegate :foo`.  Common
shape: a function `F` takes a callback `cb` as one of its params,
and the project-wide call graph needs an edge `cb → F` so taint can
trace through `F`'s body.

`Graph_from_AST.extract_calls` collects HOF callback edges in the same
walk as the regular call edges.  When the callee `F` resolves to a
function whose declared signature has a function-typed param at the
relevant position, the **HOF override** puts `F`'s `fn_id` on the
caller side of the callback edge instead of the enclosing function.

When the enclosing fdef of a callback is itself an anonymous lambda
inside a function that has an identifier (as in the registration
pattern `Group(func() { ... })`), signature lookups attribute the
callback's work to that enclosing function rather than to the
anonymous lambda, since the lambda has no interfile signature.

## Common to all languages: parent class lookup

`Mro.inherit_into_type_state` looks the path of a parent class up in
the following order (`resolve_parent`).

1. The first import of the class's file whose local identifier is the
   head of the path (`resolve_parent_qn`): its target followed by the
   rest of the path, or the target of a re-export of it
   (`chase_reexport`), when that is a class of the index.
2. The lookup that `parent_resolution` selects.  `Parent_in_own_scope`
   looks the path up in the scope that contains the class only
   (`resolve_parent_in_own_scope`).  `Parent_by_lexical_scope` looks it
   up in the enclosing scopes of the class, from the innermost to the
   root (`resolve_parent_lexical`): for the class `a.b.C` and the path
   `Base`, it tries `a.b.Base`, `a.Base` and `Base`, and takes only a
   class whose qn is one of these.
   `Parent_by_lexical_scope_then_simple_name` does the same and then,
   when no enclosing scope defines the class, takes a class whose simple
   identifier is the last part of the path (`resolve_parent_by_scope`):
   among the classes whose qn ends with the path, or, when there are
   none, among all the classes with that identifier, the one that
   shares the most leading qn parts with the class, and on a tie the
   one with the shortest qn.  A class that shares no leading qn part
   with the class is not taken.
3. When these find no class, a wildcard import of the file whose module
   defines the class (`resolve_parent_through_wildcard`).  The import
   must bind the head of the path (`Reexports.wildcard_binds`): it is
   not a re-export that binds nothing in its file (`export * from`), it
   does not hide the identifier (Scala `import a.{Base => _, _}`), and
   the module exports the identifier to a wildcard import (`__all__`,
   `wildcard_import_omits_underscore_names`).
4. Then an import whose local identifier is the head of the path, read
   through the module that the local refers to
   (`resolve_parent_through_import`).  An unaliased `import a.b` binds
   `a` (`module_bound_by_import`), so the path `a.b.Base` is the class
   `Base` of the module `a.b`.

In steps 3 and 4 the class that an import target refers to is found by
`Mro.chase_class`: the target itself, the target of a re-export of it,
or the class that a wildcard re-export of its module binds
(`Reexports.build_wildcard_reexports`).  The `chased` table of
`inherit_into_type_state` keeps the result for each target, so a chain
of re-exports is walked once per target.

Where several imports in step 3 or 4, or several wildcard re-exports of
one module, give a class, `single_binding` decides which one binds.
Where `imports_bind_in_statement_order` holds (Python), only the
imports before the class statement take part in steps 3 and 4.  The two
steps are then one: the candidates of the wildcard imports and of the
other imports are taken together in statement order, and the last one
binds (`tests/interfile/python/later_of_star_and_named_import_binds_parent/`).
An import of the identifier that resolves to no class of the index still
rebinds it, so the class of an earlier wildcard import is then not the
parent
(`tests/interfile/python/named_import_after_star_import_rebinds_parent/`).
Elsewhere step 3 comes before step 4, and the candidates of a step bind
only when they are one and the same class; two different classes bind
through neither.

Where `superclass_is_never_a_mixin` holds (Dart), a mixin is not a
candidate for a parent in the superclass position
(`class_parent.cp_is_superclass`) in step 2
(`tests/interfile/dart/superclass_is_not_a_mixin_of_same_identifier/`).

Fixtures:

- `tests/interfile/python/own_class_parent_ahead_of_star_import/` and
  `tests/interfile/java/same_package_parent_ahead_of_on_demand_import/`:
  a class `Base` of the file or of its package is the parent, not the
  class `Base` of the module of a wildcard import.
- `tests/interfile/python/star_import_binds_parent/` and
  `tests/interfile/java/on_demand_import_binds_parent/`: the parent is
  the class `Base` of the module of the wildcard import, not a class
  `Base` of another module.
- `tests/interfile/scala/hidden_name_not_bound_by_wildcard_parent/`,
  `tests/interfile/python/dunder_all_star_import_not_parent/` and
  `tests/interfile/typescript/export_star_does_not_bind_parent/`: a
  wildcard import that hides the identifier, a module whose `__all__`
  leaves the identifier out, and `export * from` bind no parent.
- `tests/interfile/python/later_star_import_binds_parent/`: of two star
  imports that define `Base`, the later one binds it;
  `tests/interfile/python/star_import_after_class_not_parent/`: a star
  import after the class statement does not bind its parent.
- `tests/interfile/python/package_init_star_reexport_parent/`: with
  `from proj.pkg import Base`, the parent is the class `Base` of
  `proj/pkg/models.py`, which `proj/pkg/__init__.py` re-exports through
  `from .models import *`, not the class `Base` of `vendor/models.py`.
- `tests/interfile/python/absolute_import_below_source_root_parent/`:
  `from app.base import Base` in `backend/app/views.py` gives the parent
  `Base` of `backend/app/base.py`, not that of `vendor/base.py`.
- `tests/interfile/python/dotted_import_parent/`: after `import a.b`,
  the parent `a.b.Base` is the class of `a/b.py`, not that of `c/b.py`.
- `tests/interfile/apex/extends_class_in_other_file/` and
  `tests/interfile/swift/subclass_of_class_in_other_file/`: the parent
  of `Sub` is the class `Base` of another file, not the class `Other`.

## Common to all languages: lambdas assigned to identifiers

- In the per-file and per-project scopes, a lambda binds the identifier
  it is assigned to only where
  `Index_lang_rules.assigned_function_defines_identifier` holds (R, where
  `f <- function(x) ...` defines `f`), and only when the target of the
  assignment is an identifier (`Scope_binding.assigned_function_bindings`):
  `tests/interfile/r/assigned_function_shadows_other_file/`,
  `tests/interfile/r/nested_assigned_function_shadows_global/`, and
  `tests/interfile/r/attribute_assignment_defines_no_function/` for a
  target that is not an identifier.  In the other languages these
  bindings of a file hold no lambda:
  `tests/interfile/python/class_body_lambda_not_bound_in_method/`
  resolves `escape(x)` in a method to the imported `escape`, not to the
  class attribute `escape = lambda x: x`.  In a per-project language a
  top-level lambda is still found through the definitions of the project
  (`Scope_binding.top_level_bindings`), and the other scope builders bind
  a lambda under the identifier of its entity
  (`Scope_binding.own_alias_bindings`).
- Where definitions of one identifier form an overload group
  (`Index_lang_rules.forms_overload_groups`: `Lang_config.overloads_by_type`,
  or a project-wide top-level scope), `Structural_dispatch.emit_overload_edges`
  keys a lambda by the identifier its entity binds, so it joins the group
  of that identifier; an anonymous lambda joins no group.
  `tests/interfile/lua/global_lambda_defined_in_two_files/` merges two
  global `handler` definitions;
  `tests/interfile/lua/global_lambdas_of_two_names_not_merged/` keeps
  `other` out of the group of `handler`.

[Next: § 5, the user-facing CLI surface.](05-cli-and-tools.md)
