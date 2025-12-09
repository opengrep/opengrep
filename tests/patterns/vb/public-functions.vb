Class C

' ERROR:
Public Function foo()
End Function

' ERROR:
Public Function foo(x as Integer)
End Function

' ERROR:
Delegate Public MustOverride Function foo()
End Function

' ERROR:
Public Function foo(x as Integer)
  x = 2
  Return 3
End Function

' OK:
Private Function foo(x as Integer)
  End
End Function

' ERROR:
Public Sub foo(x as Integer)
End Sub

End Class
