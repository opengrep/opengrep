class C
{
    void test1(params T[] args)
    {
        for (i = 0; i <= 10; i++)
        {
            // ruleid: taint
            sink(args[i]);
        }
    }

    void runTest1()
    {
        test1("abc", source(), "xyz");
    }
}
