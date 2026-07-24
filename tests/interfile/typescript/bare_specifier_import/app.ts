// BARE import specifier (no leading "./"): "lib/handler" is not a relative
// path, so projidx resolves it through the path-suffix index by matching the
// 2-segment suffix "lib/handler" against project files.  The suffix index only
// indexes suffixes up to the longest bare specifier's segment count, so this
// exercises that cap at length 2: if the cap dropped the "lib/handler" suffix,
// the import would not resolve, `new Handler()` could not bind to lib/handler.ts,
// and the cross-file taint chain through `h.process(userInput)` would break.
import Handler from "lib/handler";

interface Req { query: { [k: string]: string } }

export function handle(req: Req) {
  // SOURCE: user-controlled query param.
  const userInput = req.query.q;
  const h = new Handler();
  return h.process(userInput);
}
