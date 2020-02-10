
com =..\..\inc
msg =..\..\messages
inc     =..\..\inc
extasw  =-I$(inc) -D$(COUNTRY)

#--------------------------------------
#-- MAKEFILE for KEYB utility
#--------------------------------------
bins    =..\..\binaries
cprs    =..\..\compress
dest    =keyb.com
cpr     =keyb.co_

!IFNDEF       BUILDER

all: $(dest)

!ELSE

all: $(dest) $(bins)\$(dest) $(cprs)\$(dest)

$(bins)\$(dest): $(dest)
            copy $(dest) $(bins)

$(cprs)\$(dest): $(dest)
            copy $(dest) $(cprs)

$(cprs)\$(cpr):  $(cpr)
            copy $(cpr) $(cprs)

$(cpr):       $(dest)
            $(compress) $(dest) $(cpr)

!ENDIF



KEYB.CTL: KEYB.SKL $(msg)\$(COUNTRY).MSG  

KEYBCMD.OBJ:   KEYBCMD.ASM \
              $(com)\STRUC.INC \
              $(com)\SYSMSG.INC \
              $(com)\VERSIONA.INC \
              KEYBEQU.INC  \
              KEYBSYS.INC  \
              KEYBI9.INC   \
              KEYBI9C.INC  \
              KEYBI2F.INC  \
              $(com)\KEYBSHAR.INC \
              KEYB.CTL     \
              KEYBDCL.INC  \
              KEYBTBBL.INC \
              COMMSUBS.INC \
              KEYBCPSD.INC \
              $(com)\postequ.inc  \
              $(com)\dseg.inc     \
              $(com)\MSGSERV.ASM \
              $(com)\COPYRIGH.INC



KEYBTBBL.OBJ: KEYBTBBL.ASM \
              $(com)\KEYBSHAR.INC \
              KEYBCMD.INC  

COMMSUBS.OBJ: COMMSUBS.ASM  \
              $(com)\KEYBSHAR.INC

KEYBI2F.OBJ: KEYBI2F.ASM \
              $(com)\KEYBSHAR.INC

KEYB.OBJ: KEYB.ASM        

KEYBCPSD.OBJ: KEYBCPSD.ASM  \
              $(com)\KEYBSHAR.INC

KEYBI9.OBJ: KEYBI9.ASM     \
              $(com)\KEYBSHAR.INC

KEYBI9C.OBJ: KEYBI9C.ASM   \
              $(com)\KEYBSHAR.INC

PARSER.OBJ:    parser.asm \
               $(com)\parse.asm \
               $(com)\struc.inc \
               KEYBDCL.INC \
               $(com)\PSDATA.INC 

KEYB.exc: keyb.obj     \
          parser.obj   \
          KEYBCMD.OBJ  \
          KEYBTBBL.OBJ \
          COMMSUBS.OBJ \
          KEYBI2F.OBJ  \
          KEYBCPSD.OBJ \
          KEYBI9.OBJ   \
          keyb.lnk   \
          KEYBI9C.OBJ
    LINK @keyb.lnk

$(dest):      keyb.exc


