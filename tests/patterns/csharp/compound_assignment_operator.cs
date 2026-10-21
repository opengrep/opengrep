// ERROR:
class Counter
{
    int value;
    public void operator +=(int x) { value += x; }
}

class Pair
{
    public static Pair operator +(Pair a, Pair b) { return a; }
}
