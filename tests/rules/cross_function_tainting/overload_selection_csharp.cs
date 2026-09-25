// A call selects the overload whose parameter types match the static types
// of its arguments, whatever the order of the declarations.
class Calls
{
    void G(int n)
    {
        // ruleid: overload_selection_csharp
        Sink(n);
    }

    void G(string s)
    {
        // ok: overload_selection_csharp
        Sink(s);
    }

    void Run()
    {
        int tainted = Source().Length;
        G(tainted);
        G("constant");
    }
}
