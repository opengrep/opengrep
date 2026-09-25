package p;

public class Store {
  static String s = "";
  String g = "";

  public static void writeStatic() {
    s = source();
  }

  public static String readStatic() {
    return s;
  }

  public void writeInstance() {
    g = source();
  }

  public String readInstance() {
    return g;
  }
}
