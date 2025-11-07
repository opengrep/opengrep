class C
{
    string data;

    void Process(int first, string second)
    {
        this.data = second;
    }
    void Process(int number)
    {
        this.data = number;
    }

    void Process(string input)
    {
        this.data = input;
    }

    static void Main()
    {
        C c = new C();
        string tainted = source();
        c.Process(tainted);
        // OK: csharp_method_overload not found, double method with this arity
        sink(c.data);
        c.Process(2, tainted);
        // ruleid: csharp_method_overload
        sink(c.data);
    }

    static string source() { return "tainted"; }
    static void sink(string s) { }
}
