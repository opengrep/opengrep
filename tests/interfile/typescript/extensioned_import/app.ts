import { getData } from './utils.js';

export function run(): void {
  // ruleid: extensioned-import
  sink(getData());
  // ok: extensioned-import
  sink('static');
}
