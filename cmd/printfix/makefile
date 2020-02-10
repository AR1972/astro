#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

dest    =printfix.com
bins    =..\..\binaries
cprs    =..\..\compress


!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(dest): $(dest)
            copy $(dest) $(cprs)

!ENDIF


printfix.obj: printfix.asm printfix.inc

printfix.exc: printfix.obj
	link printfix.obj,printfix.exc;

$(dest): printfix.exc

