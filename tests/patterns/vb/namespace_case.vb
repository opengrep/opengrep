' namespace names are case-insensitive

'MATCH:
Namespace Foo.Bar
End Namespace

'MATCH:
Namespace foo.bar
End Namespace

'OK: another namespace
Namespace Foo.Baz
End Namespace
