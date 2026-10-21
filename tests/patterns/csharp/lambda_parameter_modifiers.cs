class C
{
    void M()
    {
        // ERROR:
        Apply((ref x) => x + 1);
        Apply((x) => x + 1);
    }
}
