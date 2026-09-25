public class BoxProvider : Provider
{
    protected override void Fetch(string path)
    {
        // ruleid: bare-call-reaches-override-csharp
        Sink(path);
    }
}
