// Default import: rebinds the default-exported class from handler.ts
// to the local name `Handler`.  Without projidx's default-export
// tracking the `new Handler()` site below cannot resolve to
// handler.ts's class and the cross-file chain through `h.process`
// breaks.
import Handler from "./handler";

interface Req { query: { [k: string]: string } }

export function handle(req: Req) {
  // SOURCE: user-controlled query param.
  const userInput = req.query.q;
  const h = new Handler();
  return h.process(userInput);
}
