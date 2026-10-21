class C
{
    bool M(object p)
    {
        // ERROR:
        return p is var (x, y) && x == y;
    }

    bool N(object p)
    {
        return p is string s && s.Length > 0;
    }
}
