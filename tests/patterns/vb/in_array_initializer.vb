Public Class Foo
    Public Shared Sub Main()
        'ERROR: match
        Dim span As ReadOnlySpan(Of Integer) = New Integer() {1, 42, 2}
    End Sub
End Class

