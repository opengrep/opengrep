class C
{
    void M()
    {
        // The identifier rowCount holds U+00AD SOFT HYPHEN, a formatting character.
        // ERROR:
        var row­Count = Count();
        var size = Size();
    }
}
