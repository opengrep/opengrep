# 4. Per-language quirks

Every language has its own definition of "what's a class", "what's a
method", "what's in scope", "what gets called when".  projidx
encapsulates these as fields on `Index_lang_rules.t`
(`src/project_index/Index_lang_rules.mli`).  This document walks through
the differences between languages as they show up in the code — not
as a man-page, but as a tour of what makes each language hard and how
the hooks address it.

## The Index_lang_rules.t fields

| Field | What it does |
|---|---|
| `is_init_file` | Recognise package-init files (Python `__init__.py`).  Gates the re-export map. |
| `rewrite_module_path` | Strip language-specific suffixes from `foo/bar/x.py` → `foo.bar.x`.  Python rewrites trailing `/__init__` to "". |
| `module_path_from_ast` | Extract the file's module/namespace from its AST as an override to the file-path-derived module key.  Java/Kotlin `package`, C# file-scoped `namespace`, C++ outermost `namespace`, Clojure `(ns ...)`. |
| `normalize_import_specifier` | Rewrite a raw import specifier before it becomes a module key.  C/C++ strip `.h`/`.hpp`/`.hh`/`.hxx`. |
| `class_dunders_from_decorators` | Python `@dataclass` → `[__init__]`, honouring `init=False`. |
| `class_dunders_from_extends` | Python `class X(NamedTuple)` → `[__init__; __new__; __iter__; ...]`. |
| `synth_call_dunders` | Python `X = Enum(...)` / `namedtuple(...)` / `NewType(...)` are class-defining calls. |
| `inner_class_from_call` | `class X(namedtuple("Y", ...))` — extract Y and its dunders. |
| `class_body_synth_methods` | Ruby `attr_reader :foo` declares a method with no FuncDef. |
| `class_body_extra_parents` | Ruby `include Foo`, `extend Foo`, `prepend Foo`. |
| `extract_wrapper` / `wrapper_dunders` | Python PEP-681 `@dataclass_transform()`. |
| `walks_inheritance` | Python/Java/Kotlin/C#/C++ yes; Go/C/Clojure no. |
| `has_reexports` | Python `__init__.py` re-exports. |
| `include_anonymous_funcs` | Python/Go true; Ruby/TS/JS/Java/Kotlin/C#/C++ false (lambdas/blocks inflate the graph without adding signal). |
| `unqualified_scope` | `Per_file` (Python, Ruby, JS, Clojure), `Per_directory` (Go, C), or `Per_package` (Java, Kotlin, C#, C++) — see § Java/Kotlin below. |
| `discover_excludes` | TypeScript walks `tsconfig*.json` for the `exclude` array. |
| `class_def_reshape` | Coerce Ruby `module ... end` and Go `type T struct/interface` into `ClassDef` shape. |

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
original `qn`.

### Dataclass / NamedTuple synthesis

`@dataclass` on a class adds synthesised `__init__`, `__repr__`,
`__eq__` methods.  `class X(NamedTuple)` adds `__init__`, `__new__`,
`__iter__`, etc.  These methods have no AST FuncDef but are
legitimate call targets:

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
1, *not* 1 and 2 — `self` is added by the caller via implicit
receiver dispatch, not by the call expression.  The method body
reaches the receiver as `BThis`, never as a `BArg`.

`Match_tainting_mode.ml` (`is_implicit_receiver` /
`filter_implicit_receiver_params`) therefore strips the implicit
receiver from the param list: Python `self`/`cls` and Go/Rust
`ParamReceiver`.  This is done **in both intra- and interfile
modes** (the `interfile` flag is ignored) — parameter indexing and
arity must not depend on which mode produced the signature, or
effects extracted in one mode won't line up with the params built in
the other.  Keeping it uniform aligns `BArg` indices with the
actuals at every call site: direct construction, `super().__init__`,
and plain method calls alike.

## Go

### Packages and `Per_directory` scope

Go has no `import alias` for same-package references: a file
`foo/util.go` declaring `func Helper()` is callable from
`foo/main.go` as bare `Helper()`.  `unqualified_scope =
`Per_directory` so that resolving an unqualified name in
`foo/main.go` sees every func in every file under `foo/`.

For cross-package references like `pkg.Func()`, projidx maintains
`funcs_by_package : (package_name, funcs) Hashtbl.t` and resolves
`pkg.Func` through it.

### `type T interface { ... }`

Go's tree-sitter parser does **not** emit `ClassDef` for an
interface.  It emits:

```
DefStmt(entity, TypeDef { tbody = NewType (TyRecordAnon (Interface, fields)) })
```

The default `iter_no_id_info` visitor doesn't recurse into the
`TyRecordAnon` field list, so without intervention the interface's
methods would never get `current_class` set.  Two pieces of glue
keep them visible:

- `class_def_reshape` returns a synthetic `(entity, ClassDef)` pair
  for these `TypeDef NewType TyRecordAnon (Interface, ...)` shapes
  so the regular visitor sees them.
- A specific branch in `Visit_function_defs` walks the field list
  manually when it sees a Go interface TypeDef, save-excursion'ing
  `current_class` to the interface name for each field method.

Result: interface methods get `fn_id = [Some Iface; Some method]`
and land in `Type_state.methods[Iface]`.

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

Embedded interfaces inherit all methods.  Parse-wise, the embed is a
**Spread** expression field (`G.F (ExprStmt (Call (IdSpecial Spread,
[Arg name])))`), **not** a `DefStmt FuncDef`.  A naive walk of
`Client` would see only `Name` and `IsEnabled` — `Authenticate`
would be missing.

projidx builds a project-wide `embeds : interface_name → embedded_iface_name list`
map by walking Type_def observations, then BFS-transitively gathers
every inherited func_info and re-emits it under the embedder's class
name (with `fn_id` rewritten to `[Some Embedder; Some method]`).
`find_methods(Client, Authenticate)` resolves to `Authenticator`'s
FBDecl.

### Cross-package homonym disambiguation

A Go project can easily have 30+ packages each defining
`type Service struct { ... }` with a `Get(...)` method.  When a
caller writes `s.Get(...)` where `s` is `*foo.Service`, the resolver
picks the right `Get` via this pipeline:

1. **Same-file filter:** prefer candidates in the caller's file.
2. **Same-dir filter:** prefer candidates in the caller's directory
   (= same Go package).
3. **Interface match:** if `s`'s declared type is an interface,
   route to its FBDecl.
4. **`pick_by_arity` fallback:** match on parameter count.

The pipeline is applied both at the catchall `DotAccess` branch in
`identify_callee` and at the bare-`Id` receiver branch — `s.Get(...)`
in Go can match either shape depending on whether `s` is a name or a
dotted access.

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
function-shaped AST node.  If we made every block a vertex, vertex
count and per-file work would blow up by ~10× while adding little
signal (the block bodies are usually trivial).
`include_anonymous_funcs = false`.

The block not being a vertex does **not** mean the *call* is
ignored: `f(real_args) do |x| ... end` (Ruby/Crystal/Scala) parses
as `Call(Call(f, real_args), [block])`, and `identify_callee`
resolves the edge against the inner call so `f` is still linked,
with the block collected as a HOF callback (see
[§ 6 B9](06-subtleties.md)).

### RSpec spec files

projidx uses class+method dispatch, not the name-only matching that
some other indexers (notably `scip-ruby`) use.  In RSpec-heavy
codebases this is a precision benefit: `scip-ruby` will happily link
a test stub's `def create` to every real `create` in the application
code, and a `shared_examples` block's contents to every spec that
includes it.  projidx does not make these matches.

Practically this means recall and precision metrics framed against
SCIP are best measured on non-spec code: full-tree numbers
under-report projidx because the "missed" edges are mostly the noisy
spec edges projidx is intentionally not emitting.

## TypeScript / JavaScript

### `tsconfig.json` excludes

`tsconfig*.json` files declare `exclude` arrays.
`Index_lang_rules.discover_excludes` walks for these files starting at
the project root and accumulates their `exclude` patterns
(prefers `tsconfig.build.json` over `tsconfig.json` to match
`scip-typescript`).  Patterns are glob-expanded via `Re.Glob`.

### Default and named exports

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

Two indexes keep these resolutions O(1):

- `default_export_class : (module_path, name)` — for `import X from
  "./y"`.
- `named_export_classes : (module_path, leaf_name, name)` — for
  `import { X } from "./y"`.

`I_namespace` (`import * as X from "./y"`) is recorded but
resolution is best-effort.

### Lambda VarDefs

```javascript
const callback = () => doStuff();
list.forEach(callback);
```

Lambda VarDefs (`const X = () => ...`) are valid call targets and
must be in the per-file visibility set so call sites in the same
file resolve them by name.  The Phase 1 vertex collection and the
per-file `funcs_by_name` visibility filter both treat `Func_def`,
`Class_def`, and `Var_def` (with a lambda RHS) uniformly via the
`Walker.Observation.t` variant.

## Rust

### `impl Foo { fn bar() }`

Rust's parser emits this as

```
DefStmt(ent, OtherDef("Impl", [T(Foo); Ss(stmts)]))
```

`Visit_function_defs` only matches on `ClassDef`, so without
intervention the methods inside the `impl` block don't get
`current_class = Some Foo`.  A Rust-only AST pre-pass in
`Project_index.collect` rewrites every `OtherDef("Impl", ...)` into

```
DefStmt({name=Foo}, ClassDef{cbody=stmts; ...})
```

with synthetic class metadata (empty extends, empty implements,
empty params).  Both `collect_in_ast` and `Visit_function_defs`
see the methods bound to the impl'd type.

The rewrite is `Index_lang_rules.rust_class_def_reshape`, shared by
the collector and Main's stored-AST pre-pass.  The stored-AST
application is **Rust-only**: Go and Ruby also wire
`class_def_reshape`, but only for the collector's view — their
stored `TypeDef`s/`ModuleDef`s must survive for the interface
embedding and module walks.

## PHP

PHP runs largely through the default config, with two field-typing
hooks: `strip_field_sigil` drops the `$` so `$this->x` and promoted
ctor params share one field namespace, and `ctor_param_promotion`
registers every typed constructor param as a candidate field (PHP 8
promotes them; the parser drops the visibility modifier).  Field
types otherwise come from the shared self-assignment pass.  The main
resolution twist:

### `Class::staticMethod()` vs `Class->instanceMethod()`

PHP's static-call syntax `Class::staticMethod(...)` must resolve to a
static method on `Class` specifically — not to a same-named instance
method on an unrelated class.  The obj.method() site in
`identify_callee` therefore returns `None` (rather than a bare-name
project-wide fallback) when the imported-name lookup fails:
returning a candidate without a class-context check would let
`Class::staticMethod(...)` aliases to methods on completely
unrelated classes that happen to share the leaf name.  The static
resolution path is `import name → class qualifier → exact static
method`, with no name-only fallback.

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

Same-package files are visible to each other without any `import`,
*regardless of where they sit on disk*.  The directory and the
package declaration are independent — `Per_directory` is only a
loose approximation.

projidx handles this with two pieces:

- `module_path_from_ast` plugs in `extract_package_decl`, which
  walks the top-level for a `G.Package(_, parts)` directive and
  returns the dotted name.  When `Some "com.example"`, that
  overrides the file-path-derived module path.
- `unqualified_scope = `Per_package` keys the cross-file visibility
  index on `fi_module_path` instead of the file's parent directory.
  Two files declaring `package com.example;` end up under the same
  scope key whether they live in `src/main/java/` or `tests/fixtures/`.

### Inheritance walk

Java/Kotlin classes can `extends Animal` / inherit `Animal`'s
methods.  `walks_inheritance = true` triggers `Mro.inherit_into_type_state`
which attaches the parent's methods to the subclass.

The MRO walk early-exits for subclasses with **no own methods**
(it derives the child's class identity from `Type_state.get_methods`).
A test subclass like `class Dog extends Animal {}` won't inherit
unless it has at least one declared method.  This is a known
limitation of the current Mro implementation.

### Lambdas

Stream chains (`.filter(x -> ...)`) and lambdas in callbacks are
common in Java 8+ and Kotlin.  Treating each as a vertex inflates
the graph without adding signal — same policy as TypeScript.
`include_anonymous_funcs = false`.

### `import` resolution

The Java/Kotlin tree-sitter parsers emit
`G.ImportFrom(_, DottedName ["com"; "example"], [("Foo", None)])`
for `import com.example.Foo;`, which the existing
`collect_imports` machinery handles unchanged.  No Java-specific
walker needed for imports — only for the package declaration.

## C#

### `namespace X.Y;` (file-scoped) vs `namespace X.Y { ... }` (block-scoped)

C# 10 introduced file-scoped namespaces:

```csharp
namespace App.Core;

class Service { ... }
```

The C# parser emits `G.Package(_, ["App"; "Core"])` for these, which
`extract_package_decl` picks up alongside the Java/Kotlin/C++ cases.

Block-scoped namespaces (`namespace X { ... }`) parse as
`G.DefStmt(_, G.ModuleDef _)`, not `G.Package`.  Block-scoped
namespaces are **not** surfaced as the file's module path; the
fallback is the file-path-derived module.  Mixed-namespace files
(legal but rare) get the file path as the key.

### `using` directives

`using System.IO;` parses as `G.ImportAll(_, G.DottedName ["System"; "IO"], _)`
(wildcard import).  `using Foo = System.Text;` parses as
`G.ImportAs(_, _, Some(Foo, _))`.  Both shapes already match the
existing `collect_imports` directives.  `using static System.Math;`
collapses to plain `ImportAll` (the parser can't distinguish from
non-static), which is good enough — `Math.PI` resolves the same way.

### Inheritance

`class Dog : Animal, IAnimal` — C# uses `:` for both base class and
interfaces, all in one list.  `walks_inheritance = true` handles
the base class lookup; interface implementation isn't structurally
distinguished and isn't walked separately.

## C++

### `namespace X { ... }` and `Per_package`

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

`extract_package_decl` picks up the **outermost** namespace as the
file's module path.  Nested namespaces (`namespace core` inside
`namespace app`) don't contribute — the file's module is just
`app`.  For typical C++ projects where one source file = one
outermost namespace, this is correct.  Anonymous namespaces and
multi-namespace files fall back to the file-path-derived module.

### `#include` resolution

`#include "handler.h"` parses as
`G.ImportAll(_, G.FileName "handler.h", _)` — the existing wildcard
import machinery handles it.  The catch: the raw specifier is
`"handler.h"` (with extension) but the corresponding source file's
module path is `handler` (path-derived with extension stripped by
`Fpath.rem_ext`).

`normalize_import_specifier = strip_c_header_ext` strips the
header suffix (`.h`/`.hpp`/`.hh`/`.hxx`) before it becomes a module
key.  System includes (`#include <stdlib.h>`) get the same
treatment but typically don't have a corresponding indexed file.

### Forward declarations

```cpp
// Main.cpp
class Animal { ... };  // forward declaration
int main() { Animal a; ... }
```

A forward declaration of `Animal` in `Main.cpp` registers a
**second** `class_info` for `Animal` (alongside the real one in
`Animal.cpp`) with **no methods**.  When `Mro.inherit_into_type_state`
resolves Dog's parent, the empty-stub Animal can win the lookup and
no methods get inherited.  Don't forward-declare classes whose
methods you need at link-resolution time; use `#include "Animal.h"`
instead.

## C

### Linkage and `Per_directory` scope

C has no `package`/`namespace` declaration.  Sibling `.c` files in
the same directory link together at the symbol level.  The closest
match is `unqualified_scope = `Per_directory` — better than
`Per_file` (which would miss every `main.c → handler.c` call) but
under-approximates cross-directory links and over-approximates
unrelated-translation-unit visibility.

### `#include "handler.h"`

C `#include`s parse exactly the same way as C++ —
`G.ImportAll(_, G.FileName, _)` — and use the same
`strip_c_header_ext` to drop the header suffix.

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
single `G.OtherDirective(("NsDirective", _), exprs)` — it doesn't
emit `G.Package` or pull out the `:require` clauses as standard
import directives.  `Parse_clojure_tree_sitter.ml` has a TODO to
emit proper `ImportAs`/`ImportFrom` directly at parse time; until
that's done, projidx walks the NsDirective itself.

`extract_clojure_ns_decl` walks the top-level for the NsDirective
and pulls the first form (the namespace name).  It returns
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

- adds `(h, app.handlers)` to `fi_imports` as an alias entry —
  `Pipeline.build_alias_to_module_qn` then makes `h` resolvable to
  the `app.handlers` module qn;
- adds `("*", app.handlers)` as a wildcard so the bulk re-export
  pass copies `app.handlers`'s free funcs into the importer's
  `funcs_by_module`.

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
...)` and `#(...)` anonymous functions are kept as vertices since
they're typically short and discrete (`include_anonymous_funcs =
true`).

### `Per_file` scope, not `Per_package`

Idiomatic Clojure is **one namespace per file** with explicit
`:require` for everything cross-file.  `Per_package` would silently
make sibling-ns files visible without `:require` — masking
missing-require bugs.  Clojure stays on `Per_file`.

## Common to all langs: HOF callback chains

Higher-order function patterns appear in every language:
TypeScript's `arr.map(cb)`, Python's `decorator`, Go's
`http.HandleFunc(path, handler)`, Ruby's `delegate :foo`.  Common
shape: a function `F` takes a callback `cb` as one of its params,
and the project-wide call graph needs an edge `cb → F` so taint can
trace through `F`'s body.

`Graph_from_AST.extract_calls` does a single-pass walk that picks up
HOF callback edges as a side effect of the regular call extraction.
When the callee `F` resolves to a function whose declared signature
has a function-typed param at the relevant position, the **HOF
override** redirects the callback edge's caller side to `F`'s
`fn_id` instead of the enclosing function.  This is the "callback
dispatched through F" convention used by external indexers; matching
it keeps semantics aligned across the comparison harness.

When the enclosing fdef of a callback is itself an anonymous lambda
inside a named function (e.g. `Group(func() { ... })` registration
patterns), signature lookups attribute the callback's work to the
enclosing named function rather than to the unnamed lambda, since
the lambda has no usable interfile signature.

[Next: § 5, the user-facing CLI surface.](05-cli-and-tools.md)
