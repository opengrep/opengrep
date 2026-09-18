import { Base } from "./base";

export class Child extends Base {
  emit() {
    // ruleid: new-uses-inherited-constructor
    sink(this.v);
  }
}
