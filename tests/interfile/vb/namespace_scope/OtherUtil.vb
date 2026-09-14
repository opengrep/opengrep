Namespace Other
    Module Util
        Sub Sink(x As String)
            Console.WriteLine(x)
        End Sub

        Sub Emit(msg As String)
            ' ok: test-namespace-scope
            Sink(msg)
        End Sub
    End Module
End Namespace
