using System.Collections.Generic;

class T
{
    static void HandlerGetPos()
    {
        Dictionary<string, string> m = new Dictionary<string, string>();
        m.Add("body", source());
        m.Add("user", "safe");
        // ruleid: taint
        sink(m.GetValueOrDefault("body"));
    }

    static void HandlerGetNeg()
    {
        Dictionary<string, string> m = new Dictionary<string, string>();
        m.Add("body", "safe");
        m.Add("user", source());
        // ok: taint
        sink(m.GetValueOrDefault("body"));
    }

    static void HandlerGetOrDefaultPos()
    {
        Dictionary<string, string> m = new Dictionary<string, string>();
        m.Add("body", source());
        // ruleid: taint
        sink(m.GetValueOrDefault("body", "fallback"));
    }

    static void HandlerGetOrDefaultTaintedDefault()
    {
        Dictionary<string, string> m = new Dictionary<string, string>();
        m.Add("body", "safe");
        // ruleid: taint
        sink(m.GetValueOrDefault("body", source()));
    }

    static void HandlerTryAddPos()
    {
        Dictionary<string, string> m = new Dictionary<string, string>();
        m.TryAdd("body", source());
        // ruleid: taint
        sink(m.GetValueOrDefault("body"));
    }

    static void HandlerTryAddNeg()
    {
        Dictionary<string, string> m = new Dictionary<string, string>();
        m.TryAdd("body", "safe");
        m.TryAdd("user", source());
        // ok: taint
        sink(m.GetValueOrDefault("body"));
    }
}
