:: 1.1
ver > ver. txt
systeminfo | find "память" > systeminfo.txt
echo list disk > list.txt
diskpart /s list.txt > diskpart.txt

:: 1.2
md test
copy /y c:\lab6\task1 c:\lab6\task1\test
cd test

:: 1.3
type c:\lab6\task1\test*.txt > type

:: 1.4
for %%i in (*.*) do if NOT %%i == type del %%i
