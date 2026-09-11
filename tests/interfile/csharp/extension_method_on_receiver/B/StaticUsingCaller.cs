using A;
using static A.Extensions;

class StaticUsingCaller {
    static string source() { return "x"; }
    static void Main() {
        Payload p = new Payload();
        p.Handle(source());
    }
}
