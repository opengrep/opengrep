// Guarded taint signatures with C# pattern matching. Each callee records a
// ToSink effect inside a switch case whose condition depends on a parameter.
// At signature instantiation the relational / combinator pattern is evaluated
// against the caller's constant argument; when every caller makes the case
// definitively non-matching the effect is dropped and no finding is emitted.
//
// Taint originates at the caller via Source() and flows into the callee as a
// parameter. Every callee has its own sink line so findings cannot be
// deduplicated across flows.

class Program
{
    static string Source() { return "taint"; }
    static void Sink(string s) { }

    // ---------- relational pattern: case < 0 (PatWhen) ----------

    static void RelNo(int code, string x)
    {
        switch (code)
        {
            case < 0:
                // ok: test-guards-relational-pattern-csharp
                Sink(x);
                break;
        }
    }
    static void CallRelNoA() { RelNo(5, Source()); }
    static void CallRelNoB() { RelNo(10, Source()); }

    static void RelYes(int code, string x)
    {
        switch (code)
        {
            case < 0:
                // ruleid: test-guards-relational-pattern-csharp
                Sink(x);
                break;
        }
    }
    static void CallRelYesA() { RelYes(5, Source()); }
    static void CallRelYesB() { RelYes(-1, Source()); }

    // ---------- combined range: case >= 0 and < 10 (AndPattern) ----------

    static void RangeNo(int code, string x)
    {
        switch (code)
        {
            case >= 0 and < 10:
                // ok: test-guards-relational-pattern-csharp
                Sink(x);
                break;
        }
    }
    static void CallRangeNoA() { RangeNo(20, Source()); }
    static void CallRangeNoB() { RangeNo(-5, Source()); }

    static void RangeYes(int code, string x)
    {
        switch (code)
        {
            case >= 0 and < 10:
                // ruleid: test-guards-relational-pattern-csharp
                Sink(x);
                break;
        }
    }
    static void CallRangeYesA() { RangeYes(20, Source()); }
    static void CallRangeYesB() { RangeYes(5, Source()); }

    // ---------- or pattern: case 1 or 2 (PatDisj) ----------

    static void OrNo(int code, string x)
    {
        switch (code)
        {
            case 1 or 2:
                // ok: test-guards-relational-pattern-csharp
                Sink(x);
                break;
        }
    }
    static void CallOrNoA() { OrNo(3, Source()); }
    static void CallOrNoB() { OrNo(4, Source()); }

    static void OrYes(int code, string x)
    {
        switch (code)
        {
            case 1 or 2:
                // ruleid: test-guards-relational-pattern-csharp
                Sink(x);
                break;
        }
    }
    static void CallOrYesA() { OrYes(3, Source()); }
    static void CallOrYesB() { OrYes(2, Source()); }
}
