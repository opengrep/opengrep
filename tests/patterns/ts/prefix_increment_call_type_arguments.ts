function bump(id: string) {
  // MATCH:
  ++counter<Stats>(id).value;
  ++gauge<Stats>(id).value;
}
