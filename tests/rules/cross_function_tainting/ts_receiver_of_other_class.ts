class Frame {
  push(msg: string): string {
    return source(msg);
  }

  localArray(): void {
    const tables: string[] = [];
    // ok: ts_receiver_of_other_class
    sink(tables.push("a"));
  }

  ownMethod(): void {
    // ruleid: ts_receiver_of_other_class
    sink(this.push("a"));
  }
}
