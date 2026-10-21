class C
{
    int M(int[] xs, int b)
    {
        // ERROR:
        var last = (xs.Length) - 1;
        var c = (int) -b;
        return last;
    }
}
