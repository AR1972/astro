#************************* makefile for messages **************************
!IFNDEF COUNTRY
COUNTRY =usa
!ENDIF


#
#######################	dependencies begin here. #########################
#

all: $(COUNTRY).idx

$(COUNTRY).idx: $(COUNTRY).msg
    c:\astro\c6ers\tools6\bin\buildidx $(COUNTRY).msg
