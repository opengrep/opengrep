import { getData } from './utils.mjs';
import { getOther } from './lib.cjs';

export function run(): void {
  // ruleid: esm-and-cjs-extensions
  sink(getData());
  // ruleid: esm-and-cjs-extensions
  sink(getOther());
  // ok: esm-and-cjs-extensions
  sink('static');
}
