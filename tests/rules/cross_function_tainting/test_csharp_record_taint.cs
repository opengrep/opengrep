record Service(string Name)
{
    // identity passthrough: taint in -> taint out
    public string Handle(string input)
    {
        return input;
    }
}

record struct Transformer(int Id)
{
    public string Pass(string data)
    {
        return data;
    }
}

class Program
{
    static string Source() { return "taint"; }
    static void Sink(string s) { }

    static void TestRecordClass()
    {
        var svc = new Service("svc");
        string r = svc.Handle(Source());
        // ruleid: csharp-record-taint
        Sink(r);
    }

    static void TestRecordStruct()
    {
        var t = new Transformer(1);
        string r = t.Pass(Source());
        // ruleid: csharp-record-taint
        Sink(r);
    }

    static void TestNoTaint()
    {
        var svc = new Service("svc");
        string r = svc.Handle("safe");
        // ok: csharp-record-taint
        Sink(r);
    }
}
