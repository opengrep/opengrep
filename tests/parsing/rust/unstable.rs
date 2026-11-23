/*
 * ============================================
 * Const traits and impls, const trait bounds
 * ============================================
 */

pub const trait Borrow<Borrowed: ?Sized> {
    fn borrow(&self) -> &Borrowed;
}

pub const unsafe trait Allocator {}

const impl<T> Default for OnceCell<T> {
    fn default() -> Self { Self::new() }
}

fn update(f: impl [const] FnOnce(u32) -> u32 + [const] Destruct) {}
fn iter(it: &mut impl [const] Iterator<Item = u8>) {}
fn bounds<T: Copy + const PartialEq, U>() where U: [const] PartialOrd<T> {}
fn tilde<T: ~const Clone>() {}

/*
 * ============================================
 * Impl restrictions
 * ============================================
 */

pub impl(self) unsafe trait ZeroablePrimitive: Sized + Copy {}
pub impl(crate) trait Sealed {}

/*
 * ============================================
 * Auto traits, final methods, trait aliases
 * ============================================
 */

pub unsafe auto trait Send {}
pub auto trait UnwindSafe {}

trait Share {
    final fn share(&self) -> Self;
}

pub trait Thin = Pointee<Metadata = ()> + PointeeSized;

/*
 * ============================================
 * Declarative macros 2.0, metavariable expressions
 * ============================================
 */

pub macro quote($($t:tt)*) {
    $($t)*
}

pub(crate) macro unsafe_impl_fd_asrefs {
    ($($t:ty),*) => {};
}

macro_rules! concat_names {
    ($name:ident, $ty:ty) => {
        fn ${concat(bench_, $name)}(b: $ty) {}
        $( ${ignore($ty)} self.${index()} )&&+
    };
}

macro_rules! tilde_matcher {
    ($a:ty, ~$b:ty) => {};
}

/*
 * ============================================
 * Const closures, super let, attributes on a let value
 * ============================================
 */

fn closures() {
    let count = iter.fold(0, const |accum, _elem| accum + 1);
    super let x = 1;
    let check = #[cold] |x: u8| x + 1;
}

/*
 * ============================================
 * Extern types, f16 and f128 literals
 * ============================================
 */

unsafe extern "C" {
    pub type Code;
}

fn floats() {
    let a = [0_f16; 5];
    let b = 1f128;
}
