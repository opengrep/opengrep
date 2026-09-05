class Helper
{
    // The safe overload is declared first: the sink one must be found
    // by its parameter type, not by declaration order.
    public static void Handle(int n)
    {
        Safe(n);
    }

    public static void Handle(string s)
    {
        // ruleid: overload-same-arity-csharp
        Sink(s);
    }

    static void Safe(object o) { }
    static void Sink(object o) { }
}
