// A callback inside a method calls another method on the receiver: that
// method's reads of a field the constructor set are reads of the receiver's
// field here, where the constructor's argument reaches it.
class Store {
  constructor(file) {
    this.file = file;
    this.other = 'b';
    this.load();
  }

  watch(emitter) {
    emitter.on(() => {
      this.save();
    });
  }

  save() {
    // ruleid: lambda_this_method_call
    sink(this.file);
    // ok: lambda_this_method_call
    sink(this.other);
  }

  load() {
    // ruleid: lambda_this_method_call
    sink(this.file);
  }
}

function start(emitter) {
  const store = new Store(source());
  store.watch(emitter);
}
