// A CLASS also named widget, with a sinking method. If widget() were
// mistaken for a constructor, x.render() would resolve here -> sink.
export class widget {
  render(q) {
    sink(q);
  }
}
