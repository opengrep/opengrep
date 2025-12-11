class C
{
  void foo()
  {
    string s = source();
    val d = s?.s;
    // ruleid: taint
    sink(d);
  }
}
