public static class C
{
    // in an extension block:
    
    // in the rule, we assume that the extended string is tainted
    extension(string s)
    {
        // MATCHES

        void taint1(int otherArg){
            // ruleid: extension_taint
            sink(s);
        }
        
        void taint2(){
            // ruleid: extension_taint
            sink(s);
        }

        void taint3(){
            val x = s;
            val y = x;
            // ruleid: extension_taint
            sink(y);
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

        // OK
        
        void ok1(){
            // ok:
            sink(t);
        }

        void ok2(){
            // ok:
            sink(sanitize(s));
        }
        
        void ok3(){
            s = sanitize(s);
            string g = s;
            // ok:
            sink(g);
        }
        
        void ok4(){ // shadowing
            string s;
            string g = s;
            // ok:
            sink(g);
        }

        void ok5(string s){ // shadowing
            string g = s;
            // ok:
            sink(g);
        }

    }

    // standalone:

    public void foo4(this string s)
    {
      // ruleid: extension_taint
      sink(s);
    }
    
}
