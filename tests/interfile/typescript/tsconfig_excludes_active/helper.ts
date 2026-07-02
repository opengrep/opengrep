// Sink-bearing helper.  If tsconfig.build.json's exclude is honoured,
// the only caller passing taint here (legacy/source.ts) is absent
// from the call graph and the sink below does NOT fire.
export function helper(x: string): string {
  // ok: tsconfig-excludes-active
  return db.query(x);
}

declare const db: { query(q: string): string };
