Public Class Test
    Public Shared Sub Main()
        ' OK:
        Foo(42, "baz")

        ' ERROR:
        Foo(bar:=42, baz:="baz")
        ' ERROR:
        Foo(baz:="baz", bar:=42)
    End Sub
End Class

