Public Class Service
  ' A member written Shared is shared
  ' ERROR:
  Public Shared Sub Written()
  End Sub

  ' An instance member is not shared
  ' OK:
  Public Sub Instance()
  End Sub
End Class

Public Module Helpers
  ' Every member of a Module is implicitly Shared
  ' ERROR:
  Public Sub Implicit()
  End Sub
End Module
