// Default-exported class living under lib/.  app.ts imports it with a BARE
// (non-relative) specifier `import Handler from "lib/handler"`, so projidx
// cannot resolve it by sibling-path expansion — it must match the specifier
// against the project's file set via the path-suffix index, which indexes the
// trailing `lib/handler` (2 segments) of this file's path.
export default class Handler {
  process(input: string): string {
    // ruleid: bare-specifier-import
    return db.query(input);
  }
}

declare const db: { query(q: string): string };
