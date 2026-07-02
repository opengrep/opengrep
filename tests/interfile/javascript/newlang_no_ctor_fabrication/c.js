// Decoy class with a same-named method, so `render` is NOT unique
// project-wide -> x.render() can only resolve via x's inferred type, not
// the unique-method-name fallback.  This isolates the constructor-
// fabrication path.
export class Decoy {
  render(q) {
    return q;
  }
}
