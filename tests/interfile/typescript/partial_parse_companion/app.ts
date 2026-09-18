import { forward } from './lib';

export function run(): void {
  const data = source();
  // ruleid: partial-parse-companion
  sink(data);
  forward(data);
}
