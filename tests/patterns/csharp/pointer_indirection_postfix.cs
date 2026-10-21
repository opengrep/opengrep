unsafe class C
{
    void M(int* p)
    {
        // ERROR:
        *p++ = 0;
        (*p)++;
    }
}
