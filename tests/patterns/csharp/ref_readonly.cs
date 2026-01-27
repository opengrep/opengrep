class C
{
    // ERROR:
    void foo(ref readonly int x)
    { bar(x); }
}
