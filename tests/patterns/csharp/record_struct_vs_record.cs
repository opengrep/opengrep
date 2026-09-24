namespace Demo
{
    public record Person(string Name, int Age)
    {
        public string Greeting => "Hi";
    }

    // ERROR:
    public record struct Point(int X, int Y)
    {
        public int Sum => X + Y;
    }
}
