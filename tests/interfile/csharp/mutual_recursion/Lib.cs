class Lib
{
    static bool Cond()
    {
        return false;
    }

    public static object P(object x)
    {
        return Q(x);
    }

    static object Q(object x)
    {
        return R(source());
    }

    static object R(object x)
    {
        if (Cond())
        {
            return P(x);
        }
        return x;
    }
}
