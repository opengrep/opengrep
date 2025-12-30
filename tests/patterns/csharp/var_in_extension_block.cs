public static class OtherClass
{
  extension(MyClass myClass)
  {
    void foo()
    {
      // ERROR:
      MyClass a = new MyClass();
      
      // ERROR:
      go(a);
      
      // ERROR:
      go(myClass);
    }
  }    
}
