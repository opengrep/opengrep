// A class declared inside a `namespace`.  Before the fix the
// `namespace Svc` scope was dropped entirely (TS had no reshape), so
// `Handler` was indexed as a bare top-level class — colliding with any
// real top-level `Handler` and losing its `Svc.` qualifier.  The fix
// treats `namespace` as a qualifier scope so this class's qn is
// `Svc.Handler`.
export namespace Svc {
  export class Handler {
    run(x: string): string {
      // ruleid: namespace-class-method
      return db.query(x);
    }
  }
}

declare const db: { query(q: string): string };
