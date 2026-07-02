// Default-exported class.  Importers write `import Handler from "./handler"`
// (no braces) to bind this class to a local name of their choosing.  Two
// indexes keep the resolution O(1) in projidx: default_export_class
// (for default imports) and named_export_classes (for named imports).
// This file exercises the default branch.
export default class Handler {
  process(input: string): string {
    // ruleid: default-export-class
    return db.query(input);
  }
}

declare const db: { query(q: string): string };
