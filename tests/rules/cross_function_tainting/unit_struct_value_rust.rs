// The name of a unit struct used as a value is an instance of the struct,
// so a variable it initialises is a receiver of that struct's type.
trait Handler {
    fn m(&self, x: String);

    fn run(&self, x: String) {
        self.m(x);
    }
}

struct A;

impl Handler for A {
    fn m(&self, x: String) {
        // ruleid: unit_struct_value_rust
        sink(x);
    }
}

struct C;

impl C {
    fn m(&self, x: String) {
        // ok: unit_struct_value_rust
        sink(x);
    }
}

fn main() {
    let a = A;
    a.run(source());
}
