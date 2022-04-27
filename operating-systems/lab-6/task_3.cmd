:: 3.1
net start > netstart.txt

:: 3.2.1
net stop dnscache

:: 3.2.2
timeout /t 5 /nobreak & net start > newnetstart.txt

:: 3.2.3
fc netstart.txt newnetstart.txt /n > fc.txt

:: 3.2.4
net start dnscache