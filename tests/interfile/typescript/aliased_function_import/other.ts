// Same-named export in a file this app never imports: the alias must bind
// the function from the file it names, not this function with the same simple name.
export function getData(x: string): void {
  // ok: aliased-function-import
  sink(x);
}
