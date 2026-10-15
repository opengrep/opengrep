' every part of a qualified name is case-insensitive

Module M
    Sub Main()
        'MATCH:
        Dim x As New System.Text.StringBuilder()

        'MATCH:
        Dim y As New system.text.stringbuilder()

        'OK: another class
        Dim z As New System.Text.Encoder()
    End Sub
End Module
