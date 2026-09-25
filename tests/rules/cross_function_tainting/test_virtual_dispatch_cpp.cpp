class Base {
public:
  virtual void handle(char *s) { }
  void hidden(char *s) { }
  void run(char *s) {
    this->handle(s);
    this->hidden(s);
  }
};

class Derived : public Base {
public:
  void handle(char *s) {
    // ruleid: test-virtual-dispatch-cpp
    sink(s);
  }
  void hidden(char *s) {
    // ok: test-virtual-dispatch-cpp
    sink(s);
  }
};

void start() {
  Derived d;
  d.run(source());
}
