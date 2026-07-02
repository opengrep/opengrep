Namespace App
    Module Mid
        Function Sanitize(s As String) As String
            Return s
        End Function

        Function Process(p As String) As String
            Return Sanitize(p)
        End Function
    End Module
End Namespace
