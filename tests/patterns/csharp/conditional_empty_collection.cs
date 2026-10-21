class C
{
    int[] M(X x)
    {
        // ERROR:
        return x.y ? [] : foo();
    }

    int[] N(X x)
    {
        return x.y ? [1] : foo();
    }
}
