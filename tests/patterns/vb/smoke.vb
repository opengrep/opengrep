Class C

 Sub foo(ByVal x As Integer)
   ' ERROR:
   s = 4
   ' ERROR:
   s = 2 + 2
   ' ERROR:
   s = 5 + 4
   ' ERROR:
   Dim x = 4
   ' ERROR:
   Dim x = 2 + 2
 End Sub

End Class
