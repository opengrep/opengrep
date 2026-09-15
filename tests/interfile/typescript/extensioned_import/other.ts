// A definition with the same simple name, never imported by app.ts: bare-name resolution is
// ambiguous, so only resolving the './utils.js' specifier connects the
// call to utils.ts.
export function getData(): string {
  return 'safe';
}
