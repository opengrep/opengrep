class C
{
    int[] M(int[] tail)
    {
        // ERROR:
        int[] a = [1, ..tail];
        int[] b = [1, 2];
        return a;
    }
}
