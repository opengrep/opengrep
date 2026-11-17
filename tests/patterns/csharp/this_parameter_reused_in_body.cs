public static class TextExtensions
{
    // ERROR:
    public static void Write(this string text)
    {
        sink(text);
    }

    // OK:
    public static void WriteLocal(this string text)
    {
        var local = Read();
        sink(local);
    }
}
