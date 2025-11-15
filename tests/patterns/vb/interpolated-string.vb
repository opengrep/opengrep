Class C

Sub foo

  ' ERROR:
  Query($"SELECT {GetUSerData() + 1}")

End Sub

End Class
