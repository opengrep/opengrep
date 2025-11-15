Public Class C
    Public Sub New(a As Integer, b As Integer)
    End Sub
End Class

Public Class Test
    Public Sub test()
        'ERROR: match
        Dim c As New C(1, 2)
    End Sub
End Class

