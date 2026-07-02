fn caller_pos() {
    handler_pos(Req { body: source(), user: "safe".to_string() });
}
