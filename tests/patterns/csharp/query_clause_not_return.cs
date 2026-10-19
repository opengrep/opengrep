using System.Linq;

// A query clause has no written return; the expression body of a written
// lambda is its value.
class C {
  void Query(string[] xs) {
    var q = from x in xs where Check(x) select Wrap(x);
  }

  void Lambda(string[] xs) {
    //ERROR: match
    var q = xs.Select(x => Wrap(x));
  }
}
