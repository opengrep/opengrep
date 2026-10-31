function check(u: unknown) {
  // MATCH:
  if (!isUser<Admin>(u)) {
    return;
  }
  if (isUser<Admin>(u)) {
    return;
  }
}
