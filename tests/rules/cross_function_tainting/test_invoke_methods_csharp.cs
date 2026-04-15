// C#: nested lambdas invoked via .Invoke()
class TestInvokeMethods {

    static void test_invoke() {
        var x = source();
        Action outer = () => {
            Action inner = () => {
                // ruleid: test-invoke-methods-csharp
                sink(x);
            };
            inner.Invoke();
        };
        outer.Invoke();
    }

    // Negative: no taint
    static void test_no_taint() {
        var x = "clean";
        Action outer = () => {
            Action inner = () => {
                // ok: test-invoke-methods-csharp
                sink(x);
            };
            inner.Invoke();
        };
        outer.Invoke();
    }

    static string source() { return "tainted"; }
    static void sink(string x) {}
}
