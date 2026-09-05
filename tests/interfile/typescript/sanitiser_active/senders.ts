export function sendClean(value: string): void {
  const safe = sanitize(value);
  // ok: sanitiser-active-ts
  sink(safe);
}

export function sendDirty(value: string): void {
  // ruleid: sanitiser-active-ts
  sink(value);
}
