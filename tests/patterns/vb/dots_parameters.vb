Public Class ParametersWithEllipsis
    Sub SomeMethod()
    End Sub

    Sub SomeMethod(notamatch As Integer)
    End Sub

    ' ERROR:
    Sub SomeMethod(m As Integer)
    End Sub

    ' ERROR:
    Sub SomeMethod(before As Integer, m As Integer)
    End Sub

    ' ERROR:
    Sub SomeMethod(m As Integer, after As Integer)
    End Sub

    ' ERROR:
    Sub SomeMethod(before As Integer, m As Integer, after As Integer)
    End Sub
End Class
