Public Class Main
    Public Function test1() As Object
        Dim str As String = "safe"
        Try
            Return could_throw()
        Catch t As Throwable
            str = source()
        End Try
        'ruleid: test
        sink(str)
        Return Nothing
    End Function

    Public Function test2() As Integer
        Dim cannot_throw As Integer = 42
        Dim str As String = "safe"
        Try
            Return cannot_throw
        ' vvv dead code
        Catch t As Throwable
            str = source()
        End Try
        'OK:
        sink(str)
        Return 0
    End Function
End Class
