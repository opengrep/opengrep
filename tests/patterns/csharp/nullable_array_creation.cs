class C
{
    object?[] M(object? item)
    {
        // ERROR:
        var a = new object?[] { item };
        var b = new object[] { item };
        return a;
    }
}
