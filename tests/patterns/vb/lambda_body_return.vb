Module M
  Sub Test()
    ' A Sub lambda returns nothing; a Function lambda's expression body
    ' is its return value.
    ' OK:
    Dim f = Sub() G(t)
    ' ERROR:
    Dim h = Function() G(t)
    Dim k = Sub()
      ' OK:
      G(t)
    End Sub
  End Sub
End Module
