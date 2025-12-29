public static class C
{
    // in the rule, we assume that the extended string is tainted
    extension(string s)
    {
        void tainted1(int a)
        {
            // ruleid: extension_taint
            sink(s);
        }
        
        void tainted2()
        {
            // ruleid: extension_taint
            sink(s);
        }
        
        void ok1()
        {
            // ok:
            sink(t);
        }
        
        void ok2(string s) // tainted s is shadowed
        {
            // ok:
            sink(s);
        }

        void ok3()
        {
            // ok:
            sink(sanitize(s));
        }
        
        // ruleid: extension_taint
        public string Prop2 => sink(s);

        public string Prop
        {
            // ruleid: extension_taint
            get => sink(s);
            // ruleid: extension_taint
            set => sink(s);
        }
    }
}
