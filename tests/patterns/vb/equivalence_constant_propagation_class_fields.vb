Class A
  Private f1 As String
  Private Shared f2 As String
  Private f3 As String = "abc"
  Public f5 As String
  Private ReadOnly f7 As String
  Shared Sub New()
    f2 = "abc"
  End Sub
  Sub New()
    f1 = "abc"
    f5 = "abc"
    f7 = "abc"
  End Sub
  Sub M()
    Dim loc As String = "abc"
    ' ERROR: match
    sink(0, loc)
    ' ERROR: match
    sink(1, f1)
    ' ERROR: match
    sink(2, f2)
    ' ERROR: match
    sink(3, f3)
    sink(5, f5)
    ' ERROR: match
    sink(7, f7)
  End Sub
End Class
