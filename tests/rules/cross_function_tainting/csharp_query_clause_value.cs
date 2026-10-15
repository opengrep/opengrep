using System.Linq;

// By the translation of query expressions into method calls, the select
// expression is the value of the clause's lambda.
class C {
  void Tainted(string[] xs) {
    var q = from x in xs select Wrap(Source());
    // ruleid: csharp_query_clause_value
    Sink(q);
  }

  void Constant(string[] xs) {
    var q = from x in xs select "safe";
    // ok: csharp_query_clause_value
    Sink(q);
  }
}
