void main(data) {
  // member-access ellipsis '. ... .' matches a chain of accesses
  // MATCH:
  foo().middle().sink(data);
  // MATCH:
  foo().sink(data);
  unrelated().other(data);
}
