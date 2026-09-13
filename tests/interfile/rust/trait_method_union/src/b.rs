pub struct Store {
    pub data: String,
}

impl Store {
    pub fn emit(&self) {
        // ok: trait-method-union
        sink(&self.data);
    }
}

fn sink(x: &str) {
    println!("{}", x);
}
