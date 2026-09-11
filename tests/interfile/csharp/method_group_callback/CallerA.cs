using A;

class CallerA {
    static string source() { return "x"; }
    static void Main() {
        Apply.Run(Handler.Handle, source());
    }
}
