class C
{
    bool M(bool ready, List<int> items, List<int> others)
    {
        // ERROR:
        var a = ready && items?.Count() == 1;
        // ERROR:
        var b = ready || items?.Count() == 0;
        var c = ready && others?.Count() == 1;
        return a;
    }
}
