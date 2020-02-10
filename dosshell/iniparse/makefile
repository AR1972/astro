hinc =..\inc
lib  =..\lib


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
#CC  = cl -c -DCC -DNOMINMAX -W3 -AM -Zip -Gc -DSMM -I$(hinc)
#   Non-Debuggin compile line:
CC  = cl -c -DCC -DNOMINMAX -W3 -AM -Zp -Ox -Gc -DSMM -DNDEBUG -I$(hinc)
#
#   Assembler options:
#       always:
#       -MX preserve case of globals
#       -D?SMALL small model
#       Debugging:
#       -DDEBUG define debug for all asm code
#
#   Debugging assembler line:
ASM = masm -MX -D?SMALL -DDEBUG
#   Non-Debugging assembler line:
#ASM = masm -MX -D?SMALL
#
#   Linker options:
#       /NOE no external dictionary for library search
#       /MAP generate a map file
#   Linker line:
#LINK= link /NOE/MAP/CO
LINK= link /NOE/MAP
#
#
#   OBJS is a list of all .obj files that should be linked in:
OBJS = iniparse.obj  lex.obj symbols.obj
#
#   LIB is a list of libraries that should be linked in, in order:
LIB = $(lib)\dcow3.lib
#
#   Compile all C files in makefile's directory:
#



########################### International Changes #######################

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dosshell\iniparse

#########################################################################






goal:  iniparse.obj  lex.obj symbols.obj 

*.obj:          $(hinc)\prot.h symbols.h parse.h

.c.obj:
		$(CC) $*.c 


symbols.obj:    $(MSG_DIR)\symbols.c  
		$(CC) $(MSG_DIR)\symbols.c 



