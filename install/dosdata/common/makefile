
bins    =..\..\..\binaries
cprs    =..\..\..\compress


!IFNDEF       BUILDER

all:

!ELSE

all:          $(bins)\autoexec.bat $(cprs)\autoexec.bat

$(bins)\autoexec.bat: autoexec.bat
              copy autoexec.bat $(bins)

$(cprs)\autoexec.bat: autoexec.bat
              copy autoexec.bat $(cprs)

!ENDIF
