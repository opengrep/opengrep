public abstract class Base {
    public abstract void handle(String x);

    public void process(String x) {
        this.handle(x);
    }
}
