using System.Collections.Generic;

class T
{
    static void SameKeyRoundTrip()
    {
        var m = new Dictionary<string, string>();
        m["body"] = source();
        m["user"] = "safe";
        // ruleid: taint
        sink(m["body"]);
    }

    static void SiblingKeyMustNotContaminate()
    {
        var m = new Dictionary<string, string>();
        m["body"] = "safe";
        m["user"] = source();
        // ok: taint
        sink(m["body"]);
    }
}
