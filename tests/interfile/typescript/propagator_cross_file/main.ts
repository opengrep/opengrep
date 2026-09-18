import { stash } from './helper';

export function run(): void {
  const box: string[] = [];
  stash(box, source());
  // ruleid: propagator-cross-file-ts
  sink(box);
}
