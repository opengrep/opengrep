Namespace App
    Module Impl
        Sub Sink(x As String)
            Console.WriteLine(x)
        End Sub

        Sub Emit(data As String)
            ' ok: test-sanitiser
            Sink(data)
        End Sub
    End Module
End Namespace
