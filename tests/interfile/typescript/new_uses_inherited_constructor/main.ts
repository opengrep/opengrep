import { Child } from "./child";

export function go() {
  const c = new Child(source());
  c.emit();
}
