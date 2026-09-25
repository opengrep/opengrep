public class BaseParser
{
    public void Fetch(string metadataFile, int token)
    {
        Fetch(metadataFile, "settings", token);
    }

    protected virtual void Fetch(string metadataFile, string settings, int token)
    {
    }
}

public class EpisodeParser : BaseParser
{
    protected override void Fetch(string metadataFile, string settings, int token)
    {
        // ruleid: inherited-overload-past-override-csharp
        Sink(metadataFile);
    }
}
