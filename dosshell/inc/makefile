########################### International Changes #######################
MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dosshell\inc
#########################################################################


.SUFFIXES: 
.SUFFIXES: .des .hs .sdm

.des.hs:
	de21 -g $*.des

.des.sdm:
	de21 -g $*.des


HS =    about.hs adddialo.hs addgroup.hs addprog.hs advanced.hs         \
	assoc2.hs associat.hs attr.hs attr0.hs color.hs copy.hs         \
	creatdir.hs critnew.hs dconfirm.hs delfiles.hs                  \
	delitem.hs dispopt.hs err.hs fileopts.hs                        \
	help.hs locate.hs mconfirm.hs                                   \
	message.hs mswarn.hs new.hs outofmem.hs password.hs             \
	rconfirm.hs rename.hs run.hs screen.hs showinfo.hs              \
	user.hs warn.hs

SDM =   about.sdm adddialo.sdm addgroup.sdm addprog.sdm advanced.sdm    \
	assoc2.sdm associat.sdm attr.sdm attr0.sdm color.sdm copy.sdm   \
	creatdir.sdm critnew.sdm dconfirm.sdm delfiles.sdm              \
	delitem.sdm dispopt.sdm err.sdm fileopts.sdm                    \
	help.sdm locate.sdm mconfirm.sdm                                \
	message.sdm mswarn.sdm new.sdm outofmem.sdm password.sdm        \
	rconfirm.sdm rename.sdm run.sdm screen.sdm showinfo.sdm         \
	user.sdm warn.sdm

all: dfiles build


build:  $(HS) $(SDM)
	


# Delete all not R files - from previous build
# Have to copy, otherwise put *.hs *.sdm to $(MSG_DIR)
dfiles:
	del *.des
	del *.hs
	del *.sdm
	copy $(MSG_DIR)\*.* 

