namespace P
{
    public class Store
    {
        public static string G = "";
        public string g = "";

        public static void WriteStatic()
        {
            G = Source();
        }

        public void WriteInstance()
        {
            g = Source();
        }

        public static void DirectStatic()
        {
            WriteStatic();
            // ruleid: test-bare-fields-csharp
            Sink(G);
        }

        public void DirectInstance()
        {
            WriteInstance();
            // ruleid: test-bare-fields-csharp
            Sink(g);
        }

        public void ShadowParam(string g)
        {
            g = Source();
        }

        public void Shadowed()
        {
            ShadowParam("x");
            // ok: test-bare-fields-csharp
            Sink(this.g);
        }

        public void BareWriteThisRead()
        {
            WriteInstance();
            // ruleid: test-bare-fields-csharp
            Sink(this.g);
        }

        public static void OnObjects()
        {
            var a = new Store();
            var b = new Store();
            a.WriteInstance();
            // ruleid: test-bare-fields-csharp
            Sink(a.g);
            // ok: test-bare-fields-csharp
            Sink(b.g);
        }
    }
}
