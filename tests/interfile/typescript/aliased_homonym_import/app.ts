import { A } from './a';
import { A as AB } from './b';

export function run(): void {
  // The receiver is b.ts's A, imported under an alias.  Narrowing that
  // keys the import's LOCAL name against bare class names would bind
  // this call to a.ts's method instead.
  const ab = new AB();
  ab.m(source());
}

export function unused(): A {
  return new A();
}
