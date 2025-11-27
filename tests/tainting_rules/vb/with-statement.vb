' This tests if taint is propagated via the hidden variable in With

Sub foo

Dim x = source()

'ruleid: with
sink(x.getData())

Dim y = x

'ruleid: with
sink(y.getData())

With x
  'ruleid: with
  sink(.getData())
End With

End Sub
