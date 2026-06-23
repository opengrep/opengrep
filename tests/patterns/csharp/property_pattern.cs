public class Test
{
    public bool Check(string s)
    {
        // ERROR:
        bool a = s is { Length: 5 };
        // different value in the property pattern; must not match
        bool b = s is { Length: 3 };
        return a && b;
    }
}
