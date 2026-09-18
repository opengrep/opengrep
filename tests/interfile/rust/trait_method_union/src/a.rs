pub struct Store {
    pub data: String,
}

pub trait Sink {
    fn emit(&self);
}

impl Sink for Store {
    fn emit(&self) {
        // ruleid: trait-method-union
        sink(&self.data);
    }
}

fn sink(x: &str) {
    println!("{}", x);
}
