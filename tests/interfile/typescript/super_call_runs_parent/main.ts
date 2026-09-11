import { Child } from "./child";

export function go() {
  const c = new Child();
  c.handle(source());
}
