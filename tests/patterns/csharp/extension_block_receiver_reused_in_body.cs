public static class TextExtensions
{
    extension(string text)
    {
        // ERROR:
        public void Write()
        {
            sink(text);
        }

        // OK:
        public void WriteLocal()
        {
            var local = Read();
            sink(local);
        }
    }
}
