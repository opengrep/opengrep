// RISK / regression guard for the homonym-dispatch fix.  app.ts imports the
// SAFE Handler (safe.ts); an unrelated danger.ts defines a same-named Handler
// whose process() sinks.  The fix narrows h.process() to the IMPORTED module
// (safe.ts), so NO finding may fire.  If the narrowing instead misrouted to
// danger.ts, or unioned both candidates, danger.ts's db.query sink would fire
// on its `ok:` line and this test would go red -- catching an over-broad fix.
import Handler from "./safe";

interface Req { query: { [k: string]: string } }

export function handle(req: Req) {
  // SOURCE: user-controlled query param.
  const userInput = req.query.q;
  const h = new Handler();
  return h.process(userInput);
}
