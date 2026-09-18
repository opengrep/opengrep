Namespace App
    Module Util
        Sub Sink(x As String)
            Console.WriteLine(x)
        End Sub

        Sub Emit(msg As String)
            ' ruleid: test-namespace-scope
            Sink(msg)
        End Sub
    End Module
End Namespace
