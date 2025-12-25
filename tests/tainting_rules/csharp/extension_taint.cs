public static class C
{
    // in an extension block:
    
    // in the rule, we assume that the extended string is tainted
    extension(string s)
    {
        void foo1(int aaaa){
            // ruleid: extension_taint
            sink(s);
        }
        
        void foo2(){
            // ok:
            sink(t);
        }
        
        void foo3(){
            // ruleid: extension_taint
            sink(s);
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

    // standalone:

    public void foo4(this string s)
    {
      // ruleid: extension_taint
      sink(s);
    }
    
}
