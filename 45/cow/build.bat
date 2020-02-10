set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
..\tl\bin\cl -c %CCFLAGS% -I. strings.c
..\tl\bin\lib cowusa.lib -strings.obj+strings.obj,,cow.lib

