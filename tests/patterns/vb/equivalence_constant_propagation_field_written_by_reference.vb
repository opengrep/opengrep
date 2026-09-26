Class A
  Private f1 As String = "abc"
  Private f2 As String = "abc"
  Private f3 As String = "abc"
  Sub Init()
    SetIt(f1)
    Me.SetIt(f2)
    Keep(f3)
  End Sub
  Sub M()
    sink(1, f1)
    sink(2, f2)
    ' ERROR: match
    sink(3, f3)
  End Sub
  Shared Sub SetIt(ByRef p As String)
    p = "zzz"
  End Sub
  Shared Sub Keep(ByVal p As String)
    p = "zzz"
  End Sub
End Class
