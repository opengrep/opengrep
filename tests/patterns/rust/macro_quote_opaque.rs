// https://github.com/opengrep/opengrep/pull/855
fn f(user: String) {
    //ERROR: match
    println!("{}", sink(user));
    quote! { sink(user) };
    quote::quote! { sink(user) };
    quote_spanned! { sink(user) };
    parse_quote! { sink(user) };
    syn::parse_quote! { sink(user) };
    vec![quote!(sink(user))];
    vec![parse_quote!(sink(user))];
}
