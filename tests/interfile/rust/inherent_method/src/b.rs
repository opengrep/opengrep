pub struct Store {
    pub data: String,
}

impl Store {
    pub fn send(&self) {
        // ok: inherent-method
        sink(&self.data);
    }
}

fn sink(x: &str) {
    println!("{}", x);
}
