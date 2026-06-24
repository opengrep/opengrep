public class Test
{
    public bool Check(object obj)
    {
        // ERROR:
        bool a = obj is not null;
        return a;
    }
}
