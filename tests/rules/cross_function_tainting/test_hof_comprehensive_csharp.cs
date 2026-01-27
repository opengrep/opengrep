// Comprehensive HOF test for C#: Custom and built-in higher-order functions
// All of these should detect taint flow from source() to sink()

using System;
using System.Collections.Generic;
using System.Linq;

class TestHOF
{
    // ===== Custom HOF Functions =====

    static List<T> CustomMap<T>(List<T> arr, Func<T, T> callback)
    {
        var result = new List<T>();
        foreach (var item in arr)
        {
            result.Add(callback(item));
        }
        return result;
    }

    static void CustomForEach<T>(List<T> arr, Action<T> callback)
    {
        foreach (var item in arr)
        {
            callback(item);
        }
    }

    static void DirectCall<T>(Action<T> callback, T value)
    {
        callback(value);
    }

    // ===== Test Cases =====

    static void test_custom_map()
    {
        var arr = new List<string> { source() };
        CustomMap(arr, (x) =>
        {
            // ruleid: test-hof-taint
            sink(x);
            return x;
        });
    }

    static void test_custom_foreach()
    {
        var arr = new List<string> { source() };
        CustomForEach(arr, (x) =>
        {
            // ruleid: test-hof-taint
            sink(x);
        });
    }

    static void test_direct_call()
    {
        DirectCall((x) =>
        {
            // ruleid: test-hof-taint
            sink(x);
        }, source());
    }

    // ===== Built-in LINQ methods =====

    static void test_builtin_select()
    {
        var arr = new List<string> { source() };
        arr.Select(x =>
        {
            // ruleid: test-hof-taint
            sink(x);
            return x;
        }).ToList();
    }

    static void test_builtin_where()
    {
        var arr = new List<string> { source() };
        arr.Where(x =>
        {
            // ruleid: test-hof-taint
            sink(x);
            return true;
        }).ToList();
    }

    static void test_builtin_foreach()
    {
        var arr = new List<string> { source() };
        arr.ForEach(x =>
        {
            // ruleid: test-hof-taint
            sink(x);
        });
    }

    // ===== Complex Example =====

    static string getHistory(string name, string owner)
    {
        string result = source();
        return result;
    }

    static void test_original_example()
    {
        var history = getHistory("name", "owner");
        new List<string> { history }.SelectMany(node =>
        {
            var changes = node;
            // ruleid: test-hof-taint
            sink(changes);
            return new[] { changes };
        }).ToList();
    }

    // Stub functions
    static string source() => "tainted";
    static void sink(string s) { }

    // NOTE: Top-level HOF tests not supported in C# because static constructors
    // create cycles with implicit constructor edges in the call graph.
}
