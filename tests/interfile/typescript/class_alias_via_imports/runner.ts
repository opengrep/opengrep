import { process as p } from "./lib";

export class Runner {
  // Aliased import: resolves through fi_imports (p -> lib.process).
  handler = p;
}

export class Cleaner {
  // `cleanup` is not defined in this file and not imported: the alias
  // must stay unresolved instead of binding unrelated.ts's cleanup.
  handler = cleanup;
}
