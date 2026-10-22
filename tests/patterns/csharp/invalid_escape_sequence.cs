class C
{
    string M(string name)
    {
        // ERROR:
        var path = Path.Combine("\\SHARE\images", name);
        return Path.GetFileName(path);
    }
}
