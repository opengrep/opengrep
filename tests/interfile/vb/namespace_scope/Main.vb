Namespace App
    Module Main
        Function Source() As String
            Return "tainted"
        End Function

        Sub Run()
            Dim t = Source()
            Emit(t)
        End Sub
    End Module
End Namespace
