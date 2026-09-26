using System.Collections.Generic;
using System.Linq;

public class R
{
    public R(string path, string other)
    {
        Path = path;
        Other = other;
    }

    public string Path { get; set; }
    public string Other { get; set; }
}

public class T
{
    public void Iterated()
    {
        var r = new R(source(), "x");
        var infos = new List<R>();
        infos.Add(r);
        foreach (var i in infos)
        {
            // ruleid: collection_element_fields_csharp
            sink(i.Path);
            // ok: collection_element_fields_csharp
            sink(i.Other);
        }
    }

    public void ElementRead()
    {
        var infos = new List<R>();
        infos.Add(new R(source(), "x"));
        var e = infos.ElementAt(0);
        // ruleid: collection_element_fields_csharp
        sink(e.Path);
        // ok: collection_element_fields_csharp
        sink(e.Other);
    }

    public void Indexed()
    {
        var infos = new List<R>();
        infos.Add(new R(source(), "x"));
        // ruleid: collection_element_fields_csharp
        sink(infos[0].Path);
        // ok: collection_element_fields_csharp
        sink(infos[0].Other);
    }

    public void Callback()
    {
        var infos = new List<R>();
        infos.Add(new R(source(), "x"));
        infos.ForEach(i =>
        {
            // ruleid: collection_element_fields_csharp
            sink(i.Path);
            // ok: collection_element_fields_csharp
            sink(i.Other);
        });
    }
}
