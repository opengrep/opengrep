Namespace App
    Module Impl
        Sub Sink(x As String)
            Console.WriteLine(x)
        End Sub

        Sub Leak(x As String)
            ' ruleid: test-passthrough
            Sink(x)
        End Sub
    End Module
End Namespace
