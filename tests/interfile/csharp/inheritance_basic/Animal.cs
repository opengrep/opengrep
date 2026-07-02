using System;

class Animal
{
    public void Process(string data)
    {
        // ruleid: test-inheritance-basic
        sink(data);
    }

    static void sink(string x)
    {
        Console.WriteLine(x);
    }
}
