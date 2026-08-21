public class Main {
    public void run() {
        Impl impl = new Impl();
        impl.process(source());
        // ok: abstract-self-call
        sink("static");
    }
}
