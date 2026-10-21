class C
{
    void M(Query q, int pageSize, int pageIndex)
    {
        // ERROR:
        q.Skip(pageSize * pageIndex);
        q.Skip(pageSize + pageIndex);
    }
}
