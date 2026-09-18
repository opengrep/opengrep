using A;
using _ = A.Util;

class Caller {
    static string source() { return "x"; }
    static void Main() {
        Util.Run(source());
    }
}
