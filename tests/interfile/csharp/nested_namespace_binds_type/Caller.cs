using A.B;

class Caller {
    static string source() { return "x"; }
    static void Main() {
        Util.Run(source());
    }
}
