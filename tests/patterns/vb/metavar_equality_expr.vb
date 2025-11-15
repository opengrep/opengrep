Public Class Test
    Public Shared Sub Test()
        ' ERROR:
        If (1 + 2) >= (1 + 2) Then
            Console.WriteLine("matched")
        Else
            Console.WriteLine("not matched")
        End If
        
        ' OK:
        If (1 + 2) >= (1 + 3) Then
            Console.WriteLine("matched")
        Else
            Console.WriteLine("not matched")
        End If
    End Sub
End Class

