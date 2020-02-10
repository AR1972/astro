
# -- Tools for COW --

goal: dos_machine
tools: xenix_machine


DOS_EXE = cowstrip.exe cowseg.exe echotime.exe mkdrv.exe
XENIX_EXE = cowstrip cowseg echotime mkdrv
COWSEG_O = cowseg.$O
COWSTRIP_O = cowstrip.$O misc.$O
ECHOTIME_O = echotime.$O
MKDRV_O = mkdrv.$O

.c.obj:
	$(TL)\cl -c $(TFLAGS) $*.c

.c.o:
	$(TL)\cl -c $(TFLAGS) $*.c

.asm.obj:
	$(TL)\masm -Mx $*.asm;

## - DOS machine
dos_machine: $(DOS_EXE)

## - Xenix Machine
xenix_machine: $(XENIX_EXE)


## - cowstrip :

cowstrip.exe: $(COWSTRIP_O)
	$(TL)\cl $(TFLAGS) $(COWSTRIP_O)

cowstrip: $(COWSTRIP_O)
	$(TL)\cl $(TFLAGS) -o cowstrip $(COWSTRIP_O)

$(COWSTRIP_O): cowstrip.h


## - cowseg :
cowseg.exe: $(COWSEG_O)
	$(TL)\cl $(TFLAGS) $(COWSEG_O)

cowseg: $(COWSEG_O)
	$(TL)\cl $(TFLAGS) -o cowseg $(COWSEG_O)


## - echotime :
echotime.exe: $(ECHOTIME_O)
	$(TL)\cl $(TFLAGS) $(ECHOTIME_O)

echotime: $(ECHOTIME_O)
	$(TL)\cl $(TFLAGS) -o echotime $(ECHOTIME_O)


## - mkdrv :
mkdrv.exe: $(MKDRV_O)
	$(TL)\cl $(TFLAGS) $(MKDRV_O)

mkdrv: $(MKDRV_O) ..\inc\cow.h ..\inc\indrv.h
	$(TL)\cl $(TFLAGS) -o mkdrv $(MKDRV_O)

