class C
{
    public string Message
    {
        // ruleid: taint_prop_field
        get => sink(field);
        //ruleid: taint_prop_field
        set => sink(field);
    }
    public string Message2
    {
        // ruleid: taint_prop_value
        set => sink(value);
    }
}
