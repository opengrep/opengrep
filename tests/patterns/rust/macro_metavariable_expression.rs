// MATCH:
macro_rules! with_meta {
    ($name:ident) => {
        fn ${concat(bench_, $name)}() {}
    };
}

macro_rules! without_meta {
    ($name:ident) => {
        fn concat(bench_, $name)() {}
    };
}
