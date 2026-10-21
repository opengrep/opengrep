#region Body
class C
{
    void M(int a)
    {
        // ERROR:
        Process(a);
        Other(a);
    }
}
#endregion