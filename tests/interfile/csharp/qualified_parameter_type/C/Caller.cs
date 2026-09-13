namespace C
{
    class Caller
    {
        static string source()
        {
            return "tainted";
        }

        static void Use(A.Store s)
        {
            s.Run(source());
        }
    }
}
