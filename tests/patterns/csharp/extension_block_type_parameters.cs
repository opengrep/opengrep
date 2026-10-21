static class E
{
    extension<T>(IEnumerable<T> source)
    {
        // ERROR:
        public bool HasAny() { return source.Any(); }
    }

    extension(string text)
    {
        public bool HasText() { return text.Length > 0; }
    }
}
