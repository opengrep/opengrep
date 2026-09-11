using A;

class Caller {
    static string source() { return "x"; }
    static void Main() {
        Payload p = new Payload();
        p.Handle(source());
    }
}
