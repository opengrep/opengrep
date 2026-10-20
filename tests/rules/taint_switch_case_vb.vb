Class SwitchCase
    Sub CaseDoesNotFallThrough(k As Integer)
        Dim y = 0
        Select Case k
            Case 1
                y = taint_source()
            Case 2
                ' ok: taint-switch-case-vb
                sink(y)
        End Select
    End Sub

    Sub CaseElseDoesNotFallThrough(k As Integer)
        Dim y = 0
        Select Case k
            Case 1
                y = taint_source()
            Case Else
                ' ok: taint-switch-case-vb
                sink(y)
        End Select
    End Sub

    Sub LaterCaseIsReached(k As Integer)
        Dim x = taint_source()
        Select Case k
            Case 1
                x = 0
            Case 2
                ' ruleid: taint-switch-case-vb
                sink(x)
        End Select
    End Sub

    Sub CaseReachesTheEndOfTheSelect(k As Integer)
        Dim y = 0
        Select Case k
            Case 1
                y = taint_source()
            Case 2
                y = 1
        End Select
        ' ruleid: taint-switch-case-vb
        sink(y)
    End Sub
End Class
