public class FieldFirst {
    private int count;

    public String count() {
        return "";
    }

    void use() {
        // ruleid: call-is-the-method
        count();
        // ruleid: read-is-the-field
        sink(count);
    }
}

class MethodFirst {
    public String tally() {
        return "";
    }

    private int tally;

    void use() {
        // ruleid: call-is-the-method
        tally();
        // ruleid: read-is-the-field
        sink(tally);
    }
}

class CallBeforeBoth {
    void use() {
        // ruleid: call-is-the-method
        total();
        // ruleid: read-is-the-field
        sink(total);
    }

    private int total;

    public String total() {
        return "";
    }
}
