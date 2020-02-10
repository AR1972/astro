# Makefile for Tiny swapped CW

CCPARM = -c $(CCFLAGS) $(DEFS) -NT

# -- INIT
color.$O: color.c
	cc $(CCPARM) INIT color.c
defcolor.$O: defcolor.c
	cc $(CCPARM) INIT defcolor.c

# -- USER segment
dirlist.$O: dirlist.c
	cc $(CCPARM) USER dirlist.c
dlgutil.$O: dlgutil.c
	cc $(CCPARM) USER dlgutil.c
edit.$O: edit.c
	cc $(CCPARM) USER edit.c
general.$O: general.c
	cc $(CCPARM) USER general.c
listbox.$O: listbox.c
	cc $(CCPARM) USER listbox.c
menu.$O: menu.c
	cc $(CCPARM) USER menu.c
menu2.$O: menu2.c
	cc $(CCPARM) USER menu2.c
parse.$O: parse.c
	cc $(CCPARM) USER parse.c
scroll.$O: scroll.c
	cc $(CCPARM) USER scroll.c
sdm.$O: sdm.c
	cc $(CCPARM) USER sdm.c
sdmcab.$O: sdmcab.c
	cc $(CCPARM) USER sdmcab.c
sdmtmc.$O: sdmtmc.c
	cc $(CCPARM) USER sdmtmc.c

# Some modules will go into special user segment, that must be
# locked in memory if we are running off of a floppy disk.  These
# functions are needed to display the message box to tell the user
# to reinsert their disk.
box.$O: box.c
	cc $(CCPARM) USERMSG box.c
button.$O: button.c
	cc $(CCPARM) USERMSG button.c
dlgcore.$O: dlgcore.c
	cc $(CCPARM) USERMSG dlgcore.c
helpstub.$O: helpstub.c
	cc $(CCPARM) USERMSG helpstub.c
keystub.$O: keystub.c
	cc $(CCPARM) USERMSG keystub.c
menucore.$O: menucore.c
	cc $(CCPARM) USERMSG menucore.c
msgbox.$O: msgbox.c
	cc $(CCPARM) USERMSG msgbox.c
static.$O: static.c
	cc $(CCPARM) USERMSG static.c
trapstub.$O: trapstub.c
	cc $(CCPARM) USERMSG trapstub.c
window.$O: window.c
	cc $(CCPARM) USERMSG window.c

# All other go into the CORE segment
