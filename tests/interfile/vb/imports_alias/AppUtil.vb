Namespace App
    Module Util
        Sub Sink(x As String)
            Console.WriteLine(x)
        End Sub

        Sub Emit(msg As String)
            ' ok: test-imports-alias
            Sink(msg)
        End Sub
    End Module
End Namespace
