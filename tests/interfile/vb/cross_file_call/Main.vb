Namespace App
    Module Main
        Function Source() As String
            Return "tainted"
        End Function

        Sub Run()
            Dim tainted = Source()
            Impl.Greet(tainted)
        End Sub
    End Module
End Namespace
