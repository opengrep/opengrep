// Definitions with the same simple names, never imported by app.mts:
// bare-name resolution is ambiguous, so only resolving the './utils.mjs'
// and './lib.cjs' specifiers connects the calls to utils.mts and lib.cts.
export function getData(): string {
  return 'safe';
}

export function getOther(): string {
  return 'safe';
}
