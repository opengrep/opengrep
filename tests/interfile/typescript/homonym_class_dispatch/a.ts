// The class app.ts actually imports.  Its process() is the real sink.  Even
// though b.ts defines a same-named `class Handler`, narrowing methods to the
// imported file (narrow_methods_by_import_files) keeps only this file's
// Handler.process at dispatch, so the cross-file taint flows and the finding
// fires here.
export default class Handler {
  process(input: string): string {
    // ruleid: homonym-class-dispatch
    return db.query(input);
  }
}

declare const db: { query(q: string): string };
