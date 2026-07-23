class Client : B.Base {
    static string source() { return "x"; }
    static void main() {
        string t = source();
        var c = new Client();
        c.handle(t);
    }
}
