class C
{
    void M(int a, int b)
    {
#if DEBUG
        if (a > 0)
#else
        if (b > 0)
#endif
        {
            // ERROR:
            Process(a);
        }
        Other(b);
    }
}
