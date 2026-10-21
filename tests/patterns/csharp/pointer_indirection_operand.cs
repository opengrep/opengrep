unsafe class C
{
    void M(byte* b, int* p, int i)
    {
        // ERROR:
        var w = *(int*)b;
        // ERROR:
        *(int*)b = 1;
        var v = *(p + i);
        *(p + 1) = 3;
    }
}
