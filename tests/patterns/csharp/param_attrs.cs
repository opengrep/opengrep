class A
{
  // OK:
  void test1(int x) { }

  // ERROR:
  int test2(ref readonly int y) { }

  // ERROR:
  int test3(int x, ref readonly int y) { }

  // OK:
  int test4(int x, in int y) { }
}
