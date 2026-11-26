import java.util.Optional;

class UnknownCallWithLambda {
  void tainted() {
    Optional<String> value = Optional.ofNullable(source());
    Optional<String> other = value;
    value = value.or(() -> other);
    // ruleid: java-unknown-call-with-lambda
    sink(value.get());
  }

  void clean() {
    Optional<String> value = Optional.ofNullable("constant");
    Optional<String> other = value;
    value = value.or(() -> other);
    // ok: java-unknown-call-with-lambda
    sink(value.get());
  }
}
