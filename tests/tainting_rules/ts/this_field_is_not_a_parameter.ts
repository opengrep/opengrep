class C {
  foo(http: string) {
    this.http = source();
    // ok: this_field_is_not_a_parameter
    sink(http);
  }

  bar(http: string) {
    this.http = source();
    // ruleid: this_field_is_not_a_parameter
    sink(this.http);
  }
}
