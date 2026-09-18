import { A } from "./lib";

declare function source(): string;

export function use(s: A.Store): void {
  s.run(source());
}
