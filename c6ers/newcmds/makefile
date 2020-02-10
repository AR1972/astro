#		       MS-DOS 6 "NewCmds" MASTER MAKE FILE
#
#   Builds sources in NEWCMDS and subdirectories.
#
#   To build all .EXE's                      -  nmake
#   To build all real mode .EXE's            -  nmake real
#   To build from one source, one .EXE       -  nmake sourcename
#
#   To add a new single source utility:
#
#	1. Put the source file in this directory.
#       2. Add the .exe name to each .EXE list (REALEXES)
#	   for which an .EXE is to be built.
#	3. Add a basename-only target to the list of these names.
#	4. Add a dependency line to the appropriate lists
#
#   Modifications
#
#   HISTORY:
#     07-Nov-1992 bens  Copied from \\TOOLSVR\SOURCES\...\ZTOOLS project
#     13-Nov-1992 bens  Update to \\TOOLSVR\SOURCES\...\SYSTOOLS project,
#                           and link with SETARGV.OBJ from C RTL!

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\c6ers\newcmds

.SUFFIXES:  .c .obj .exe

!IFDEF DEBUG
DCFLAGS=/Zi
DLFLAGS=/CO
!ELSE
DCFLAGS=
DLFLAGS=
!ENDIF

# Define DOS for tools.h
GCFLAGS=-DDOS -Itoolib\h $(CFLAGS) $(DCFLAGS)

SARGV=  ..\tools6\lib\setargv.obj
ASM=    masm -D?WIN -D?PLM -Mx
LINK=   link $(LFLAGS)
MAKE=   nmake
SLIBC=

# 07-Nov-1992 bens Do we need re.mi?
# LIBS= re.mi tools libh slibcr

TOOLLIB= toolib\lib\stoolsr
LIBS=    $(TOOLLIB) slibce

#CCLINK= -link /NOD /NOE /map $(DLFLAGS) $(LFLAGS) /CP:1 $(LIBS)

CC =      cl -c -nologo -Zp -Oas -DLINT_ARGS $(GCFLAGS)
LINK=	  link /NOD /NOE /STACK:12288 /map $(DLFLAGS) $(LFLAGS) /CP:1

#
#   Add .exe ( or .com ) names here
#
REALEXES=   \
        deltree.exe     \
        move.exe        \


#
#   Here's all the work.  DO NOT PLACE ANY DEPENDANCY LINES ABOVE HERE!
#

all: tools $(REALEXES)

# Make sure tool library is built, first
tools:
    cd toolib
    nmake
    cd ..

#
#   Real Mode .EXE dependency lines
#
#   NOTE: Link with setargv.obj to get automatic wild-card expansion!
#
deltree.exe:    deltree.obj
	$(LINK) $*.obj ,$*.exe,$*.map, $(LIBS);

deltree.obj:    deltree.c deltree.msg
        $(CC) deltree.c

deltree.msg: $(MSG_DIR)\deltree.msg
        copy $(MSG_DIR)\deltree.msg

move.exe:         move.obj
	$(LINK) $*.obj ,$*.exe,$*.map, $(LIBS);

move.obj:         move.c move.msg
        $(CC) move.c

move.msg: $(MSG_DIR)\move.msg
        copy $(MSG_DIR)\move.msg
