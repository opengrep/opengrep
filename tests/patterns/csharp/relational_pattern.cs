public class Test
{
    public bool Check(int score)
    {
        // ERROR:
        bool a = score is < 5;
        // ERROR:
        bool b = score is < 100;
        // this is a relational pattern with a different operator; must not match
        bool c = score is > 0;
        return a && b && c;
    }
}
