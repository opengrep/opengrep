Public Class Test
    Public Static Sub Test
        ' ERROR:
        If x = some_cond
            Console.WriteLine("matched")
        Else
            Console.WriteLine("not matched")
        End If
        
        ' ERROR:
        If x = some_cond Then Console.WriteLine("matched")
    End Sub
End Class
