#
#	Microsoft Confidential
#	Copyright (C) Microsoft Corporation 1991
#	All Rights Reserved.
#
MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dev\smartdrv

LINK= link
NAME= safedsk
LIBRARY= 
OBJ = $(NAME).obj resbuf.obj transbuf.obj dtext.obj badpart.obj
ASM = masm

goal: $(NAME).exe

.asm.obj:
	$(ASM) $*.asm,$*.obj,$*.lst ;

dtext.asm: $(MSG_DIR)\dtext.asm
    copy  $(MSG_DIR)\dtext.asm


#
#   Make will have succeeded if make finishes with this file existing:
#
#
$(NAME).exe: $(OBJ)
	$(LINK) @<<
	$(OBJ),
	$(NAME), $(NAME),$(LIBRARY);
<<
	del dtext.asm

#$(NAME).com: $(NAME).exe
#	exe2bin $(NAME).exe $(NAME).com
#	copy $(NAME).com $(NAME).sys

iclean:
    del dtext.asm
    del dtext.obj


