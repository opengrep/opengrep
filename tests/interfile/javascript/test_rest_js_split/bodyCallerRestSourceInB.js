function bodyCallerRestSourceInB() {
  // source at position 1 binds b; rest covers [2..]
  bodyHandlerRestSourceInB(["safe", source(), "ok"]);
}
