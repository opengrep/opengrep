using System;

class Store
{
    private string data;

    public Store(string data)
    {
        this.data = data;
    }

    public void Send()
    {
        // ruleid: test-constructor-taint
        sink(this.data);
    }

    static void sink(string x)
    {
        Console.WriteLine(x);
    }
}
