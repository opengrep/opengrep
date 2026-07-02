Namespace App
    Module Impl
        Sub Sink(x As String)
            Console.WriteLine(x)
        End Sub

        Sub Greet(msg As String)
            ' ruleid: test-cross-file-call
            Sink(msg)
        End Sub
    End Module
End Namespace
