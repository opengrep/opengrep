package app;

public interface ShadowedGreeter {
    default void handle(String data) {
        // ok: default-method-after-unresolved-superclass
        sink(data);
    }
}
