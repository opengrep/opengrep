# After a SHELL instruction the RUN bodies are handed to a different shell
# parser, so exercise the bracket handling on that path too.
FROM mcr.microsoft.com/windows/servercore:ltsc2022
SHELL ["powershell", "-Command"]
RUN [ -f C:\tmp\x ]
RUN Get-ChildItem | Where-Object { $_.Name -match '[0-9]+' }
RUN ["powershell", "-Command", "Write-Host [ok]"]
