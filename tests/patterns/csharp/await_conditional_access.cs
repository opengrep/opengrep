class C
{
    async Task M(Worker? worker)
    {
        // ERROR:
        await worker?.Run();
        await worker.Run();
    }
}
