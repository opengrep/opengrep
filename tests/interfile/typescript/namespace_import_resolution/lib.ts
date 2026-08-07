// Named export consumed via a namespace import on the other side.
// If projidx's namespace-import resolution covers `lib.process`,
// the cross-file edge into this sink is built.
export function process(x: string): string {
  // ruleid: namespace-import-resolution
  return db.query(x);
}

declare const db: { query(q: string): string };
