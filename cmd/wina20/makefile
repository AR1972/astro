#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */

#*************************** Makefile for DOS ***************************

bins    =..\..\binaries
cprs    =..\..\compress


#
#######################	dependencies begin here. #########################
#

!IFNDEF       BUILDER

all:

!ELSE

all: $(bins)\wina20.386 $(cprs)\wina20.38_


$(bins)\wina20.386: wina20.386
                    copy wina20.386 $(bins)

wina20.38_:         wina20.386
                    $(compress) wina20.386 wina20.38_

$(cprs)\wina20.38_: wina20.38_
                    copy wina20.38_ $(cprs)

!ENDIF
