// A value pushed into a field of an object taints that field, not the value
// the object was built from.
export default class B {
  async enable() {
    this.root().innerHTML = await this.render();
    setTimeout(async () => {
      await this.update();
    });
  }
  async render(actions) {
    const data = { count: this.items.length, actions: [] };
    actions.forEach((trigger) => {
      data.actions.push({ t: source() });
    });
    // ruleid: js_push_into_object_does_not_taint_its_initialiser
    sink(data.actions);
  }
  async update() {
    const text = await getString(this.items.length);
    // ok: js_push_into_object_does_not_taint_its_initialiser
    sink(text);
  }
}
