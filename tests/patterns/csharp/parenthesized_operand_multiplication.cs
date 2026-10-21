unsafe class C
{
    int M(int a, int b, int* p)
    {
        // ERROR:
        var m = (a) * b;
        var v = (int) *p;
        return m;
    }
}
