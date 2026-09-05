class Program
{
    static void Run()
    {
        Senders.SendClean(source());
        Senders.SendDirty(source());
    }
}
