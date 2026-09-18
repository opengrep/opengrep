export function run(x: string) {
  // ok: side-effect-import-binds-nothing
  sink(x);
}
