class C
{
    string M(string name)
    {
        // ERROR:
        var path = Path.Combine("C:\users\me", name);
        return Path.GetFileName(path);
    }
}
