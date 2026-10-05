// https://github.com/opengrep/opengrep/pull/855
// A cast is not run in a macro that only emits or reads its tokens.
fn f(n: u64) {
    //ERROR: match
    println!("{}", n as u32);
    quote! { n as u32 };
    stringify!(n as u32);
    matches!(n as u32, 0..=9);
}
