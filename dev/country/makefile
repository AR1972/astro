#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */
#************************ Makefile for Country.sys ************************

inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)
bins    =..\..\binaries
cprs    =..\..\compress
dest    =country.sys
cpr     =country.sy_

#
######################## Dependencies begin here ##########################
#

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(dest)

$(cprs)\$(dest): $(dest)
            copy $(dest) $(cprs)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)


!ENDIF

country.obj: country.asm mkcntry.inc

country.exe: country.obj
	link country;

country.sys: country.exe
	exe2bin country.exe country.sys


