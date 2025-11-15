Class C

Sub foo

  ' ERROR:
  Query("SELECT" & SomethingElse() & GetUserData())

  ' ERROR:
  Query("SELECT" & SomethingElse() & SomethingElse() & GetUserData())

End Sub

End Class
