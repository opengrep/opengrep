using B;

class CallerB {
    static string source() { return "x"; }
    static void Main() {
        Util.Run(source());
    }
}
