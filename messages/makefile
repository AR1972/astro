#************************* makefile for messages **************************
!IFNDEF COUNTRY
COUNTRY =usa
!ENDIF


#
#######################	dependencies begin here. #########################
#

all: $(COUNTRY).idx $(COUNTRY)-ibm.idx

$(COUNTRY).idx: $(COUNTRY).msg
    c:\astro\c6ers\tools6\bin\buildidx $(COUNTRY).msg
	
$(COUNTRY)-ibm.idx: $(COUNTRY)-ibm.msg
    c:\astro\c6ers\tools6\bin\buildidx $(COUNTRY)-ibm.msg
	
clean:
	del $(COUNTRY).idx
	del $(COUNTRY)-ibm.idx
