Sub Foo()

  ' ERROR:
  x = Function(x) 42
  
  ' ERROR:
  y = Function(x)
        Return 42
      End Function

  ' ERROR:
  z = Function(x)
        DoSth()
        Return 42
      End Function

End Sub
