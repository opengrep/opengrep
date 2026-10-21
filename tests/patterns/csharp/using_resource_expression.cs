class C
{
    async Task M(Context context, Context other)
    {
        // ERROR:
        await using (context.ConfigureAwait(false))
        {
            Work();
        }
        await using (other)
        {
            Work();
        }
    }
}
