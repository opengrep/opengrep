pub struct Store {
    pub data: String,
}

pub trait Left {
    fn emit(&self);
}

pub trait Right {
    fn emit(&self);
}

impl Left for Store {
    fn emit(&self) {
        // ruleid: trait-method-ambiguous
        sink(&self.data);
    }
}

impl Right for Store {
    fn emit(&self) {
        // ruleid: trait-method-ambiguous
        sink(&self.data);
    }
}

fn sink(x: &str) {
    println!("{}", x);
}
