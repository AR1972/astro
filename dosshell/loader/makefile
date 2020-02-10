#   Compiler options:
#   Always:
#        -c   compiler should not link
#        -W2  warning level 2 (max)
#        -AM  medium model code..near data, far code
#        -Zp  pack structures on byte boundaries
#        -Gc  Pascal calling conventions
#   Debugging:
#        -Zi  codeview information
#        -DDEBUG define DEBUG for all code
#
#   Non-Debugging:
#        -Ox  total optimization
#             includes:
#             -Oa ingnore aliasing
#             -Oi intrinsic functions
#             -Ol loop optimizations
#             -Ot optimize for speed
#             -Gs disable stack checking
#
#   Debugging compile line:
INCL = loader


########################### International Changes #######################

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dosshell\loader

#########################################################################


CC  = cl -c -W2 -AM -Zip -Gc
#   Non-Debuggin compile line:
#CC  = cl -c -W2 -AM -Zp -Ox -Gc
#
#   Assembler options:
#       always:
#       -MX preserve case of globals
#       -D?SMALL small model
#       Debugging:
#       -DDEBUG define debug for all asm code
#
#   Debugging assembler line:
#ASM = masm -MX -D?SMALL -DDEBUG -I$(INCL) -I$(MSG_DIR)
#   Non-Debugging assembler line:
!IFDEF ROMDOS
ASM = masm -MX -D?SMALL -DROMDOS -I$(INCL) -I$(MSG_DIR)
!ELSE
ASM = masm -MX -D?SMALL -I$(INCL) -I$(MSG_DIR)
!ENDIF

QASM = masm -MX -D?SMALL -DQLOADER -I$(INCL) -I$(MSG_DIR)
#
#   Linker options:
#       /NOE no external dictionary for library search
#       /MAP generate a map file
#   Linker line:
LINK= link /NOE/MAP
LIBRARY = ;
#
#   Name is the name of the linker's output .exe file:
NAME = tstshell
#
#   OBJS is a list of all .obj files that should be linke in:
OBJS = loader.obj tstshell.obj
OBJSQ = qloader.obj
#
#   LIBRARY is a list of libraries that should be linked in, in order:
LIBRARY =
#
#
#
#   Compile all C files in makefile's directory:
.c.obj:
	$(CC) $*.c
#
#   Assemble all ASM files in makefile's directory, generate listing:
.asm.obj:
	$(ASM) $*.asm,$*.obj,$*.lst ;



bins    =..\..\binaries
cprs    =..\..\compress
dest    =dosshell.com
cpr     =dosshell.co_

!IFNDEF       BUILDER

all: tstshell.exe $(dest) ..\$(dest)

!ELSE

all: $(dest) $(cprs)\$(cpr) $(bins)\$(dest)

$(bins)\$(dest): $(dest)
	    copy $(dest) $(bins)

$(cprs)\$(cpr):  $(cpr)
	    copy $(cpr) $(cprs)

$(cpr):       $(dest)
	    $(compress) $(dest) $(cpr)


!ENDIF

..\$(dest): $(dest)
	    copy $(dest) ..


qloader.exe:    $(OBJSQ)
	$(LINK) @<<
$(OBJSQ)
$*,
$*,
$(LIBRARY)
NUL;
<<


tstshell.exe:   $(OBJS)
	$(LINK) @<<
$(OBJS)
$*,
$*,
$(LIBRARY)
NUL;
<<


loader.obj:     loader.asm swtch_re.inc swtch_tr.inc

qloader.obj:    loader.asm swtch_re.inc swtch_tr.inc
		$(QASM) loader,qloader;

$(dest):        qloader.exe
		reloc qloader.exe $(dest)
		
