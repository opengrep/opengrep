class App
{
    static void Main()
    {
        // The source is injected inside the cycle P -> Q -> R -> P and
        // must come around as P's return value.
        // ruleid: mutual-recursion-csharp
        sink(Lib.P(0));
    }
}
