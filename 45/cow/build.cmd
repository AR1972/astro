set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
..\tl\binb\cl -c %CCFLAGS% -I. strings.c
..\tl\binb\lib cowusa.lib -strings.obj+strings.obj,,cow.lib

