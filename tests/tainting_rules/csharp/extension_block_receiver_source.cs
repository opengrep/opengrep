public static class TextExtensions
{
    extension(string text)
    {
        public void Write()
        {
            // ruleid: extension_block_receiver_source
            sink(text);
        }

        public void WriteTimes(int count)
        {
            // ruleid: extension_block_receiver_source
            sink(text);
        }

        public void WriteLocal()
        {
            var local = Read();
            // ok: extension_block_receiver_source
            sink(local);
        }
    }
}
