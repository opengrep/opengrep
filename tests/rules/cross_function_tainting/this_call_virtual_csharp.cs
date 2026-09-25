// A call on this reaches the overrides of a virtual method; a method that
// is not virtual is bound at compile time, and a new method only hides it.
class Base
{
    public void Run(string x)
    {
        this.M(x);
    }

    public virtual void M(string x)
    {
    }
}

class Sub : Base
{
    public override void M(string x)
    {
        // ruleid: this_call_virtual_csharp
        sink(x);
    }
}

class PlainBase
{
    public void Run(string x)
    {
        this.M(x);
    }

    public void M(string x)
    {
    }
}

class Hiding : PlainBase
{
    public new void M(string x)
    {
        // ok: this_call_virtual_csharp
        sink(x);
    }
}

class Program
{
    static void Main()
    {
        new Sub().Run(source());
        new Hiding().Run(source());
    }
}
