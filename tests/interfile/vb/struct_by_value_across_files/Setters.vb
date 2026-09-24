Namespace App
    Module Setters
        Function Source() As String
            Return "tainted"
        End Function

        Sub ByStruct(s As S)
            s.F = Source()
        End Sub

        Sub ByRefStruct(ByRef s As S)
            s.F = Source()
        End Sub

        Sub ByClass(k As K)
            k.F = Source()
        End Sub
    End Module
End Namespace
