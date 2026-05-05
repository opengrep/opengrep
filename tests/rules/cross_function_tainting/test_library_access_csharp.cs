// Cross-function field-sensitive taint through C#'s
// System.Collections.Generic.IDictionary library-call recognisers.
// Dictionaries flow across function boundaries as parameters /
// returns, and field-sensitivity must survive the call.

using System.Collections.Generic;

class T
{
    static void writeBody(IDictionary<string, string> m, string v)
    {
        m.Add("body", v);
    }

    static void writeUser(IDictionary<string, string> m, string v)
    {
        m.Add("user", v);
    }

    static string readBody(IDictionary<string, string> m)
    {
        return m.GetValueOrDefault("body");
    }

    static void CrossPos()
    {
        IDictionary<string, string> m = new Dictionary<string, string>();
        writeBody(m, source());
        // ruleid: test-library-access-taint
        sink(m.GetValueOrDefault("body"));
    }

    static void CrossNegSibling()
    {
        IDictionary<string, string> m = new Dictionary<string, string>();
        m.Add("body", "safe");
        writeUser(m, source());
        // ok: test-library-access-taint
        sink(m.GetValueOrDefault("body"));
    }

    static void CrossReadPos()
    {
        IDictionary<string, string> m = new Dictionary<string, string>();
        m.Add("body", source());
        // ruleid: test-library-access-taint
        sink(readBody(m));
    }
}
