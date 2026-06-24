namespace Demo
{
    // ERROR:
    public record Person(string Name, int Age)
    {
        public string Greeting => "Hi";
    }

    // ERROR:
    public record struct Point(int X, int Y)
    {
        public int Sum => X + Y;
    }

    // a regular class is not a record; must not match
    public class NotARecord
    {
        public int Z;
    }
}
