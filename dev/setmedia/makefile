#
#	Microsoft Confidential
#	Copyright (C) Microsoft Corporation 1991
#	All Rights Reserved.
#

LINK= link
NAME= setmedia
LIBRARY= 
OBJ = $(NAME).obj
ASM = masm

.asm.obj:
	$(ASM) $*.asm,$*.obj,$*.lst ;


#
#   Make will have succeeded if make finishes with this file existing:
#
goal: $(NAME).sys
#
$(NAME).exe: $(OBJ)
	$(LINK) @<<
	$(OBJ),
	$(NAME), $(NAME),$(LIBRARY);
<<
$(NAME).com: $(NAME).exe
	exe2bin $(NAME).exe $(NAME).com
$(NAME).sys: $(NAME).com
	del $(NAME).sys
	ren $(NAME).com $(NAME).sys

