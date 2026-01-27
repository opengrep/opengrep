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
        // ruleid: taint_prop_value, taint_prop_value_no_field
        set => sink(value);
    }

    extension (string s)
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
            // ruleid: taint_prop_value, taint_prop_value_no_field
            set => sink(value);
        }
    }

}
