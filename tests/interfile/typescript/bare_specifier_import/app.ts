// BARE import specifier (no leading "./"): "lib/handler" names a package,
// and the only way it reaches a project file is the tsconfig "paths" entry
// that maps "lib/*" to "./lib/*".  Without that entry the specifier binds
// nothing, `new Handler()` cannot bind to lib/handler.ts, and the
// cross-file taint chain through `h.process(userInput)` is absent.
import Handler from "lib/handler";

interface Req { query: { [k: string]: string } }

export function handle(req: Req) {
  // SOURCE: user-controlled query param.
  const userInput = req.query.q;
  const h = new Handler();
  return h.process(userInput);
}
