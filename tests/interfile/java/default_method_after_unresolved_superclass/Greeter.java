package app;

public interface Greeter {
    default void handle(String data) {
        // ruleid: default-method-after-unresolved-superclass
        sink(data);
    }
}
