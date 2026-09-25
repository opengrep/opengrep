class A {
  private f1: string;
  private static f2: string;
  private f3 = "abc";
  private f4: string;
  f5: string;
  private readonly f7: string;
  f8 = "abc";
  constructor(private p: string = "abc") { this.f1 = "abc"; this.f4 = "abc"; this.f5 = "abc"; this.f7 = "abc"; }
  other() { this.f4 = "zzz"; }
  m() {
    // ERROR: match
    const loc = "abc"; sink(0, loc);
    sink(1, this.f1);
    sink(2, A.f2);
    sink(3, this.f3);
    sink(4, this.f4);
    sink(5, this.f5);
    // ERROR: match
    sink(7, this.f7);
    sink(8, this.f8);
    sink(11, this.p);
  }
}
