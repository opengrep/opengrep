using System;

class Program
{
    static string source()
    {
        return Environment.GetEnvironmentVariable("SECRET")!;
    }

    static void Main()
    {
        string tainted = source();
        Store s = new Store(tainted);
        s.Send();
    }
}
