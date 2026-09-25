import { Handler } from "./handler";

export function use(h: Handler): void {
  h.handle(source());
}
