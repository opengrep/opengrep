// Unambiguous RELATIVE import of the Handler defined in a.ts.  The finding
// SHOULD fire: user input flows into a.ts's Handler.process -> db.query.
//
// It does NOT fire, and that is the bug this fixture pins: b.ts also defines
// `class Handler { process }`.  Method dispatch indexes methods by BARE class
// name, so `h.process()` collects BOTH Handler.process implementations, and
// pick_by_arity (Callee_resolution.ml) drops the call on the identical
// (class, method, arity) collision -- a false negative caused purely by an
// unrelated homonym class the caller never even imports.  Deleting b.ts makes
// the finding fire, which isolates the homonym as the cause.
import Handler from "./a";

interface Req { query: { [k: string]: string } }

export function handle(req: Req) {
  // SOURCE: user-controlled query param.
  const userInput = req.query.q;
  const h = new Handler();
  return h.process(userInput);
}
