Class C

Sub foo

  ' OK:
  If x = y Then
    
    ' ERROR:
    x = y
    
    ' ERROR:
    x =
      ' OK:
      y = z
    
    ' ERROR:
    x =
     ' OK:
     (y = z) And True
    
  End If
    
  ' OK:
  While x = y
  End While

End Sub

End Class
