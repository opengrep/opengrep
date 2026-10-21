class C
{
    bool M(object o, bool ready)
    {
        // ERROR:
        if (o is null && ready) { return true; }
        if (o is null) { return false; }
        return o is not null || ready;
    }
}
