public class Impl extends Base {
    public void handle(String x) {
        // ruleid: abstract-self-call
        sink(x);
    }
}
