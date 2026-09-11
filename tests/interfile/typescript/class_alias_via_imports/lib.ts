export function process(data) {
  // Reached via Runner.handler: the class file imports THIS process
  // (under the alias p), so the field alias binds here.
  // ruleid: class-alias-via-imports
  sink(data);
}
