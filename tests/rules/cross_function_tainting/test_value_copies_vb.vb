Structure S
    Public F As String
End Structure

Class K
    Public F As String
End Class

Module TestValueCopies
    Sub ByStruct(s As S)
        s.F = source()
    End Sub

    Sub ByRefStruct(ByRef s As S)
        s.F = source()
    End Sub

    Sub ByClass(k As K)
        k.F = source()
    End Sub

    Sub Caller()
        Dim a As New S()
        ByStruct(a)
        ' ok: test-value-copies-vb
        sink(a.F)
        Dim c As New S()
        ByRefStruct(c)
        ' ruleid: test-value-copies-vb
        sink(c.F)
        Dim b As New K()
        ByClass(b)
        ' ruleid: test-value-copies-vb
        sink(b.F)
    End Sub
End Module
