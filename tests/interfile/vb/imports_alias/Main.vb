Imports U = Other.Util

Namespace App
    Module Main
        Function Source() As String
            Return "tainted"
        End Function

        Sub Run()
            Dim t = Source()
            U.Emit(t)
        End Sub
    End Module
End Namespace
