using A;

class Program {
    static string source() { return "x"; }
    static void Main() {
        Child c = new Child(source());
        c.Report();
    }
}
