' ruleid: vbnet-prop-setter
Public Class Foo
    Public Property Bar As Integer
        Get
            Return 3
        End Get
        Set(ByVal value As Integer)
            Bar = value
        End Set
    End Property
End Class
