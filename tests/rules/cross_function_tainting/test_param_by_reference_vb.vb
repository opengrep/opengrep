Module TestParamByReference
    Sub SetIt(ByRef x As String)
        x = source()
    End Sub

    Sub Caller()
        Dim s As String = ""
        SetIt(s)
        ' ruleid: test-param-by-reference-vb
        sink(s)
    End Sub
End Module
