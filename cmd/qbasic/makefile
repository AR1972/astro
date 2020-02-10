#*************************** Makefile for DOS ***************************

bins    =..\..\binaries
cprs    =..\..\compress



#
####################### dependencies begin here. #########################
#

!IFNDEF       BUILDER

all:

!ELSE

all: pkl \
     $(bins)\qbasic.exe   $(cprs)\qbasic.exe   \
     $(bins)\qbasic.hlp   $(cprs)\qbasic.hl_   \
     $(bins)\edit.com     $(cprs)\edit.com     \
     $(bins)\edit.hlp     $(cprs)\edit.hl_     \
     $(bins)\help.com     $(cprs)\help.com     \
     $(bins)\help.hlp     $(cprs)\help.hl_     \
     $(bins)\help.oem     $(cprs)\help.oe_     \

pkl:
		    copy qbasic.exe qtemp.exe    
		    pklite -o qtemp.exe


$(bins)\qbasic.exe: qbasic.exe
		    copy qtemp.exe $(bins)\qbasic.exe


$(cprs)\qbasic.exe: qbasic.exe
		    copy qtemp.exe $(cprs)\qbasic.exe


$(bins)\qbasic.hlp: qbasic.hlp
		    copy qbasic.hlp $(bins)

qbasic.hl_:         qbasic.hlp
		    $(compress) qbasic.hlp qbasic.hl_

$(cprs)\qbasic.hl_: qbasic.hl_
		    copy qbasic.hl_ $(cprs)


$(bins)\msherc.com: msherc.com
		    copy msherc.com $(bins)

msherc.co_:         msherc.com
		    $(compress) msherc.com msherc.co_

$(cprs)\msherc.co_: msherc.co_
		    copy msherc.co_ $(cprs)


$(bins)\edit.com: edit.com
		    copy edit.com $(bins)

$(cprs)\edit.com: edit.com
		    copy edit.com $(cprs)


$(bins)\edit.hlp: edit.hlp
		    copy edit.hlp $(bins)

edit.hl_:         edit.hlp
		    $(compress) edit.hlp edit.hl_

$(cprs)\edit.hl_: edit.hl_
		    copy edit.hl_ $(cprs)


$(bins)\gorilla.bas: gorilla.bas
		    copy gorilla.bas $(bins)

gorilla.ba_:         gorilla.bas
		    $(compress) gorilla.bas gorilla.ba_

$(cprs)\gorilla.ba_: gorilla.ba_
		    copy gorilla.ba_ $(cprs)


$(bins)\nibbles.bas: nibbles.bas
		    copy nibbles.bas $(bins)

nibbles.ba_:         nibbles.bas
		    $(compress) nibbles.bas nibbles.ba_

$(cprs)\nibbles.ba_: nibbles.ba_
		    copy nibbles.ba_ $(cprs)


$(bins)\remline.bas: remline.bas
		    copy remline.bas $(bins)

remline.ba_:         remline.bas
		    $(compress) remline.bas remline.ba_

$(cprs)\remline.ba_: remline.ba_
		    copy remline.ba_ $(cprs)


$(bins)\money.bas: money.bas
		    copy money.bas $(bins)

money.ba_:         money.bas
		    $(compress) money.bas money.ba_

$(cprs)\money.ba_: money.ba_
		    copy money.ba_ $(cprs)

$(bins)\help.com: help.com
		    copy help.com $(bins)

$(cprs)\help.com: help.com
		    copy help.com $(cprs)


$(bins)\help.hlp: help.hlp
		    copy help.hlp $(bins)

help.hl_:         help.hlp
		    $(compress) help.hlp help.hl_

$(cprs)\help.hl_: help.hl_
		    copy help.hl_ $(cprs)


$(bins)\help.oem: help.oem
		    copy help.oem $(bins)

help.oe_:         help.oem
		    $(compress) help.oem help.oe_

$(cprs)\help.oe_: help.oe_
		    copy help.oe_ $(cprs)


!ENDIF
