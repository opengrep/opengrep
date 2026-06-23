public class Test
{
    public bool Check(int n)
    {
        // ERROR:
        bool a = n is 1 or 2;
        return a;
    }
}
