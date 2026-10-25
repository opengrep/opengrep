// Type queries do not load modules at runtime.
// ok: typescript-runtime-import-only
type Factory = typeof import("./types").factory<string>;
// ok: typescript-runtime-import-only
type Module = typeof import("./types");
// ok: typescript-runtime-import-only
type Nested = typeof import("./types").Namespace.factory<string>;
// ok: typescript-runtime-import-only
type Indexed = typeof import("./types")["factory"];
// ok: typescript-runtime-import-only
type Inline = import("./types").Container<string>;

// ruleid: typescript-runtime-import-only
const dynamic = import("./runtime");
// ruleid: typescript-runtime-import-only
const property = import("./runtime").then(useModule);
// ruleid: typescript-runtime-import-only
const runtimeTypeof = typeof import("./runtime");
