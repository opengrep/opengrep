class C
{
    void foo(Obj? obj)
    {
        // ERROR:
        obj?.x = 2;
    }
}
