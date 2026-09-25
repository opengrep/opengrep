// A call on a value whose declared type is an interface reaches the method
// of every class declared to implement that interface.
export interface Handler {
  handle(x: string): void;
}
