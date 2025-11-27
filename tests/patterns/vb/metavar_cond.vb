Public Class Test
    Public Shared Sub Test()
        'ERROR:
        If x = some_cond Then
            Console.WriteLine("matched")
        Else
            Console.WriteLine("not matched")
        End If
    End Sub
End Class

