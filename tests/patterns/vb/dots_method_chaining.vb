Public Class Foo

    Sub test()

        'ERROR: match
        f = Me.foo().m().h().bar().z()

        'ERROR: match
        f = Me.foo().bar()

        f = Me.foo().m().h().z()

        'ERROR: match $O can match o.before()
        f = Me.before().foo().m().h().bar().z()

    End Sub

End Class

