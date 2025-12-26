class C
{
  // ERROR:
  void foo1(int a) { }

  // OK:
  void foo2(string t, int a) { }

  // ERROR:
  void foo3(this string t, int a) {  }
}
