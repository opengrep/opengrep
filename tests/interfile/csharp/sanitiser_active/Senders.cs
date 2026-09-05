class Senders
{
    public static void SendClean(string value)
    {
        string safe = sanitize(value);
        // ok: sanitiser-active-csharp
        sink(safe);
    }

    public static void SendDirty(string value)
    {
        // ruleid: sanitiser-active-csharp
        sink(value);
    }
}
