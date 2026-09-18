use crate::source::source;
use crate::test4_level1::test4_level1;

fn test4_caller() {
    let x = source();
    test4_level1(x);
}
