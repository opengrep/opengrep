enum Color { Red }

class Paint
{
    public Color Color { get; set; }

    Color Pick()
    {
        Color chosen = Color;
        return chosen;
    }
}
