// Test: C# collection models for taint propagation
using System;
using System.Collections.Generic;

public class CollectionModelsTest
{
    public void TestListAdd(string tainted)
    {
        var list = new List<string>();
        list.Add(tainted);
        // ruleid: csharp-collection-taint
        Sink(list);
    }

    public void TestDictionaryAdd(string tainted)
    {
        var dict = new Dictionary<string, string>();
        dict.Add("key", tainted);
        var value = dict.GetValueOrDefault("key");
        // ruleid: csharp-collection-taint
        Sink(value);
    }

    public void TestStackPop(string tainted)
    {
        var stack = new Stack<string>();
        stack.Push(tainted);
        var value = stack.Pop();
        // ruleid: csharp-collection-taint
        Sink(value);
    }

    public void TestQueueDequeue(string tainted)
    {
        var queue = new Queue<string>();
        queue.Enqueue(tainted);
        var value = queue.Dequeue();
        // ruleid: csharp-collection-taint
        Sink(value);
    }

    private void Sink(object data)
    {
        Console.WriteLine(data);
    }
}
