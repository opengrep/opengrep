Sub foo

Dim r As Runtime = Runtime.getRuntime()

Dim command As String = getSafeCommand()
'OK:
r.exec(command)

Dim command2 As String = getUnsafeCommand()
'ruleid: os-command
r.exec(command2)

End Sub
