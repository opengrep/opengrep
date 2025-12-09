Sub X

Dim s = New SomeType With {
  .SomeField1 = 0,
  ' ERROR:
  .SomeField2 = True
  }

End Sub
