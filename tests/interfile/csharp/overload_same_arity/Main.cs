class Program
{
    static string Source() { return "x"; }

    static void Go()
    {
        Helper.Handle(Source());
    }
}
