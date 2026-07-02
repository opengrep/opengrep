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
        Dog d = new Dog();
        d.Process(tainted);
    }
}
