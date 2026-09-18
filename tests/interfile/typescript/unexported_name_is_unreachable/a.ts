function run(x: string) {
  // ok: unexported-name-is-unreachable
  sink(x);
}

export function used(x: string) {
  return x;
}
