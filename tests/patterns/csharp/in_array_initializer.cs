public class Foo {
    public static void main() {
      //ERROR: match
      ReadOnlySpan<int> span = new int[] { 1, 42, 2 };
      //ERROR: match
      ReadOnlySpan<int> span = stackalloc int[] { 1, 42, 2 };
   }
}
