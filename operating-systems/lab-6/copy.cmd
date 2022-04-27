for %%f in (C:\Windows\*) do {
    if %%~zf gtr 2097152 xcopy %% f \\%computername%\temp\%%~nf%%~xf
}