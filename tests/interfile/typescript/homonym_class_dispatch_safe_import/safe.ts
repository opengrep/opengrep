// The imported (SAFE) Handler: process() returns its input unchanged, no sink.
// Because app.ts imports THIS file, dispatch must resolve h.process() here.
export default class Handler {
  process(input: string): string {
    return input;
  }
}
