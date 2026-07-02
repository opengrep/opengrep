Namespace App
    Module Main
        Function Source() As String
            Return "tainted"
        End Function

        Sub Run()
            Dim x = Source()
            Dim y = Mid.Process(x)
            Impl.Emit(y)
        End Sub
    End Module
End Namespace
