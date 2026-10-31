const o = { new: 1 };
o.new();
class C {
  new() {}
}
interface I {
  new: number;
  new(): I;
  new (x: number): I;
}
