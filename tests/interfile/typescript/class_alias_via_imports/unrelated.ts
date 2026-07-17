export function cleanup(data) {
  // Same-named as Cleaner's field target but NEVER imported by the
  // class file: the alias must not bind here.
  // ok: class-alias-via-imports
  sink(data);
}
