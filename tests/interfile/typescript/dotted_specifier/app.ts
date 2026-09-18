import { getData } from './types.d';

export function run(): void {
  // ruleid: dotted-specifier
  sink(getData());
  // ok: dotted-specifier
  sink('static');
}
