void F(object x)
{
    switch (x)
    {
        case (int y, int z):
            // ruleid: taint
            sink(y);
            break;

        default:
            sink(5)
            break;
    }
}

void G(object x)
{
    switch (x)
    {
        case ((int y, int z), int w):
            // ruleid: taint
            sink(y);
            break;

        default:
            // ruleid: taint
            sink(x);
    }
}

void H(object x)
{
    switch (x)
    {
        // ruleid: taint
        case (int y, int z) when sink(y):
            // ruleid: taint
            sink(z);
            break;

        default:
            break;
    }
}
