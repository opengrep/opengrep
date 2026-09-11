using A;

class CallerA {
    static string source() { return "x"; }
    static void Main() {
        Util.Run(source());
    }
}
