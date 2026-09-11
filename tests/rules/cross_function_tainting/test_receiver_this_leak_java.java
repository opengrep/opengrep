public class TestReceiverThisLeakJava {

    static class Box {
        String field;

        void touch(String p) {
            p.mangle();
        }

        static void touchStatic(String p) {
            p.mangle();
        }

        void store(String p) {
            this.field = p;
        }

        String reveal() {
            return this.field;
        }
    }

    static void test_unresolved_call_in_instance_method() {
        Box b = new Box();
        b.touch(source());
        // ok: test-receiver-this-leak-java
        sink(b);
        // ok: test-receiver-this-leak-java
        sink(b.reveal());
    }

    static void test_unresolved_call_in_static_method() {
        Box b = new Box();
        Box.touchStatic(source());
        // ok: test-receiver-this-leak-java
        sink(b);
    }

    static void test_field_assignment_reaches_receiver_field() {
        Box b = new Box();
        b.store(source());
        // ruleid: test-receiver-this-leak-java
        sink(b.reveal());
    }

    static String source() { return "tainted"; }
    static void sink(String x) {}
    static void sink(Box x) {}
}
