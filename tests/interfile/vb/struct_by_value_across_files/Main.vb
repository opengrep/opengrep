Namespace App
    Module Main
        Sub Sink(x As String)
            Console.WriteLine(x)
        End Sub

        Sub Run()
            Dim a As New S()
            Setters.ByStruct(a)
            ' ok: test-struct-by-value-across-files
            Sink(a.F)
            Dim c As New S()
            Setters.ByRefStruct(c)
            ' ruleid: test-struct-by-value-across-files
            Sink(c.F)
            Dim b As New K()
            Setters.ByClass(b)
            ' ruleid: test-struct-by-value-across-files
            Sink(b.F)
        End Sub
    End Module
End Namespace
