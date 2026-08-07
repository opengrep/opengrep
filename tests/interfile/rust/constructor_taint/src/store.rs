pub struct Store {
    pub data: String,
}

impl Store {
    pub fn send(&self) {
        // ruleid: test-constructor-taint
        sink(&self.data);
    }
}

pub fn sink(x: &str) {
    println!("{}", x);
}
