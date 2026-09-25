Class A
    Private fa As Integer
    Public Property P As Integer
        Get
            Return fa
        End Get
        Set(value As Integer)
            fa = value
        End Set
    End Property
    Sub M()
        Foo(P, fa)
    End Sub
End Class
