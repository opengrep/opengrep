// Test topological ordering with classes and methods

class UserHandler {
  processInput() {
    const userInput = source();
    // ruleid: test_topo_order_classes
    sink(this.transform(userInput));
  }

  transform(data) {
    return data; // Returns tainted data
  }
}

const handler = new UserHandler();
handler.processInput();
