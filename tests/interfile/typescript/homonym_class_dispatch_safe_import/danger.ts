// A same-named Handler that DOES sink -- but app.ts never imports it.  The fix
// must resolve h to safe.ts (the imported module), so this sink must NOT fire.
// The `ok:` marker asserts no finding here; if the fix misroutes to danger.ts
// or unions both homonyms, this line fires and the test goes red.
export default class Handler {
  process(input: string): string {
    // ok: homonym-class-dispatch-safe-import
    return db.query(input);
  }
}

declare const db: { query(q: string): string };
