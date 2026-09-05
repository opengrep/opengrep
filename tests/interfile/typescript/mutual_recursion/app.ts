import { p } from './lib';

export function main(): void {
  // The source is injected inside the cycle p -> q -> r -> p and must
  // come around as p's return value.
  // ruleid: mutual-recursion-ts
  sink(p(0));
}
