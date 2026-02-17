// ERROR:
public class Child(int x) : Base(x)
{

}
// ERROR:
public class Child(int x, string y) : Base(x, y), IFoo
{

}
public class NoArgs(int x) : Base
{

}
