MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dev\smartdrv

# Chuck's additions to ScottQ's makefile, to allow Soft-Ice source
#  level debugging.  Taken from JohnHe's BIOS makefile.

#  NOTE!  We do *NOT* list .inc dependencies, so care should be taken
#         to DEL *.OBJ to make sure everything is built correctly after
#         an .inc file change.

!IF     "$(DEBUG)" == "soft_ice" || "$(debug)" == "soft_ice" || \
	"$(DEBUG)" == "SOFT_ICE" || "$(debug)" == "SOFT_ICE"


ASM = masm -DUSE_VALID -W2 -MX -D?SMALL -Zd -T
LINK= link /NOE/MAP/NOD/LI
!ELSE
ASM = masm -DUSE_VALID -W2 -MX -D?SMALL -T
LINK= link /NOE/MAP/NOD/DOSSEG
!ENDIF

CC  = cl -c -W2 -AS -Gcsw -Ox -nologo -G2
##CFLAGS = -c -G2 -Zip -AS -Ox

NAME= bambi
LIBS = 

OBJS1 = dev.obj $(NAME).obj dirtywrt.obj xms.obj cacheman.obj queueman.obj
OBJS2 = int2f.obj hooks.obj queueini.obj reboot.obj popup.obj beep.obj rtext.obj rdata.obj
OBJS3 = tdata.obj bambinit.obj umbload.obj get_dpb.obj indosbox.obj xmsini.obj
OBJS4 = cacheini.obj int2fini.obj hooksini.obj cmdline.obj text.obj dec_out.obj drvtype.obj logphys.obj devini.obj pdetect.obj stacker.obj
OBJS5 = doubleb\safedsk.obj doubleb\resbuf.obj doubleb\transbuf.obj doubleb\dtext.obj doubleb\badpart.obj

bins  = ..\..\binaries
cprs  = ..\..\compress
dest  = smartdrv.exe
cpr   = smartdrv.ex_

!IFNDEF         BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(cpr)

$(bins)\$(dest): $(dest)
	    copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
	    copy $(cpr) $(cprs)

$(cpr):          $(dest)
		 $(compress) $(dest) $(cpr)
!ENDIF

.c.obj:
	$(CC) $*.c

.asm.obj:
	$(ASM) $*.asm;

.asm.lst:
	$(ASM) -l $*.asm;

text.asm: $(MSG_DIR)\text.asm
    copy  $(MSG_DIR)\text.asm

rtext.asm: $(MSG_DIR)\rtext.asm
    copy  $(MSG_DIR)\rtext.asm

$(OBJS5):
	cd doubleb
	nmake
	cd ..   

$(dest): iclean $(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4) $(OBJS5)
	$(LINK) @<<
$(OBJS1)+
$(OBJS2)+
$(OBJS3)+
$(OBJS4)+
$(OBJS5) 
$(NAME).exe 
$(NAME)
$(LIBS)
NUL;
<<
	cd sdvxd
	del *.386
	nmake
	cd ..
	

#International mods
iclean:
    del .\doubleb\dtext.obj
    del .\doubleb\dtext.asm
    del text.asm
    del text.obj
    del rtext.asm
    del rtext.obj


#$(NAME).com: $(NAME).exc
#        exe2bin $(NAME).exc $(NAME).com

clean:
	del $(NAME).exe
	del $(NAME).exc
	del $(NAME).dos
	del *.obj
	del *.map
	del *.sym
	del *.bak

depend:
	mv makefile makefile.old
	sed "/^# START Dependencies/,/^# END Dependencies/D" makefile.old > makefile
	del makefile.old
	echo # START Dependencies >> makefile
	includes -l *.c *.asm >> makefile
	echo # END Dependencies >> makefile

# START Dependencies  
# END Dependencies  


