fn caller_neg() {
    handler_neg(Req { body: "safe".to_string(), user: source() });
}
