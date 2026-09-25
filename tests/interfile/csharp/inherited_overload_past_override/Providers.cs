public abstract class BaseProvider
{
    public void GetMetadata(string info)
    {
        Fetch(Source(info), 0);
    }

    protected abstract void Fetch(string path, int token);
}

public class EpisodeProvider : BaseProvider
{
    protected override void Fetch(string path, int token)
    {
        // EpisodeParser overrides the overload of Fetch with three
        // parameters; the one with two, called here, is inherited from
        // BaseParser.
        new EpisodeParser().Fetch(path, token);
    }
}
