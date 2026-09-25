struct Foo {
    a: i32,
}

impl Foo {
    fn make() -> Self {
        Self { a: 1 }
    }
}

trait Tr {
    fn m() -> Self;
}

impl Tr for Foo {
    fn m() -> Self {
        Self { a: 2 }
    }
}
