Class C

Sub foo

  ' ERROR:
  If x = y Then
    
    ' OK:
    x = y
    
    ' OK:
    x =
      ' ERROR:
      y = z
    
    ' OK:
    x =
      ' ERROR:
      (y = z) And True
    
  End If
    
  ' ERROR:
  While x = y
  End While

End Sub

End Class
