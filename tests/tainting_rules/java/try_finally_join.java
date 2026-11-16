public class Main {
    public void nestedConstructorThenFinally() {
        String data = "";
        Reader r = null;
        try {
            r = new Reader(open());
            data = source();
        } catch (Exception e) {
        } finally {
            close(r);
        }
        //ruleid: test
        sink(data);
    }

    public void finallyOnly() {
        String data = "";
        Object o = null;
        try {
            foo();
            o = mk();
            data = source();
        } finally {
            bar();
        }
        //ruleid: test
        sink(data);
    }

    public void catchOnly() {
        String data = "";
        Object o = null;
        try {
            foo();
            o = mk();
            data = source();
        } catch (Exception e) {
        }
        //ruleid: test
        sink(data);
    }

    public void catchAndFinally() {
        String data = "";
        Object o = null;
        try {
            foo();
            o = mk();
            data = source();
        } catch (Exception e) {
        } finally {
            bar();
        }
        //ruleid: test
        sink(data);
    }

    public void overwrittenAfterFinally() {
        String data = "";
        Object o = null;
        try {
            foo();
            o = mk();
            data = source();
        } finally {
            bar();
        }
        data = "safe";
        //ok: test
        sink(data);
    }
}
