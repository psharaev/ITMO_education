:: 2.1
echo %computername% > computername.txt

:: 2.2
copy. cmd

:: 2.3
schtasks /create /sc minute /tn copy /tr copy.cmd

::2.4
schtasks /query | find "copy"
schtasks /delete /tn copy

:: 2.5
fc C:\lab6\task2\copyfile.txt \\%computername%\copyfile.txt > fc.txt

:: 2.6
copy. cmd
