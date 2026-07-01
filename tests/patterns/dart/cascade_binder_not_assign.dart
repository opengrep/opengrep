void f(cfg, obj) {
  // A cascade is desugared by applying each section directly to the receiver
  // (there is no synthetic binder assignment), so a method-only cascade has no
  // assignment and '$X = $Y' does NOT match it...
  obj..foo()..bar();
  // ...but a real field-assignment section still matches as an assignment.
  // MATCH:
  cfg..enabled = true;
}
