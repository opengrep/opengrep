Sub Foo()

  ' ERROR:
  x = Sub(x) DoSth()
  
  ' ERROR:
  y = Sub(x)
        DoSth()
      End Sub

  ' ERROR:
  z = Sub(x)
        DoSth()
        DoSthElse()
      End Sub

End Sub

