public class Provider
{
    public void GetMetadata(string info)
    {
        // A call written without a receiver runs on the instance, so it
        // reaches the override the instance's class defines.
        Fetch(Source(info));
    }

    protected virtual void Fetch(string path)
    {
    }
}
