class Store
{
    static string s = "";
    string g = "";

    void F()
    {
        Use(s, g);
        s = "a";
        g = "b";
    }
}
