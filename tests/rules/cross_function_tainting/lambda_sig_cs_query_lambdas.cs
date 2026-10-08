// The lambdas of a query expression have no token of their own: each still
// gets its own signature.
class C {
  void M() {
    var cmd = Request.Query["c"];
    // ruleid: lambda-sig-cs-query-lambdas
    var r = from u in users where Exec(cmd) select u.Name;
  }

  void N() {
    var name = "fixed";
    // ok: lambda-sig-cs-query-lambdas
    var r = from u in users where Exec(name) select u.Name;
  }
}
