class Program {
    static void Main() {
        string tainted = Source();
        Helper.Handle(1, tainted);
        Helper.Drop(1, tainted);
    }

    static string Source() { return "tainted"; }
}
