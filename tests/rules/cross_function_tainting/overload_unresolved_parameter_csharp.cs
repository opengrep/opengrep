using Com.Missing;

// The class Missing is not in the program. It can declare an implicit
// conversion from Leaf, so the overload that takes Missing stays.
public class Leaf {}

public class Other {}

public class Overloads
{
    public void Take(Missing m, string s)
    {
        // ruleid: overload_unresolved_parameter_csharp
        sink(s);
    }

    public void Take(Other o, string s)
    {
        // ok: overload_unresolved_parameter_csharp
        sink(s);
    }

    static void sink(string x) {}
}

public class Calls
{
    public void Run(Overloads o, Leaf leaf)
    {
        o.Take(leaf, source());
    }

    static string source() { return "tainted"; }
}
