// A reference initialised with a variable is that variable.
void aliases_the_variable() {
  int a = source();
  auto f = [&r = a]() {
    // ruleid: cpp_lambda_capture_reference_initialised
    sink(r);
  };
  f();
}

// A reference initialised with a field of a variable is that field.
struct S { int b; };
void aliases_the_field() {
  S s;
  s.b = source();
  auto f = [&r = s.b]() {
    // ruleid: cpp_lambda_capture_reference_initialised
    sink(r);
  };
  f();
}

// A reference initialised with a field is not the other fields.
struct T { int b; int c; };
void aliases_only_the_field() {
  T t;
  t.c = source();
  auto f = [&r = t.b]() {
    // ok: cpp_lambda_capture_reference_initialised
    sink(r);
  };
  f();
}

// A reference initialised with a variable has its fields.
void aliases_the_fields() {
  T t;
  t.c = source();
  auto f = [&r = t]() {
    // ruleid: cpp_lambda_capture_reference_initialised
    sink(r.c);
    // ok: cpp_lambda_capture_reference_initialised
    sink(r.b);
  };
  f();
}
