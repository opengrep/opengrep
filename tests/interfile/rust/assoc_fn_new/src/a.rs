pub struct Store {
    pub data: String,
}

impl Store {
    pub fn new(data: String) -> Store {
        // ruleid: assoc-fn-new
        sink(&data);
        Store { data }
    }
}

fn sink(x: &str) {
    println!("{}", x);
}
