// Namespace import (`import * as`).  Use site below is `lib.process`
// — a DotAccess on the namespace alias.  Resolving this to the
// named export from "./lib" is what § 4 TS flags as best-effort.
import * as lib from "./lib";

interface Req { query: { [k: string]: string } }

export function handle(req: Req) {
  const userInput = req.query.q;
  return lib.process(userInput);
}
