#************************* makefile for messages **************************
!IFNDEF COUNTRY
COUNTRY =usa
!ENDIF


#
#######################	dependencies begin here. #########################
#

all: $(COUNTRY).idx

$(COUNTRY).idx: $(COUNTRY).msg
                buildidx $(COUNTRY).msg

clean:
	-del USA.IDX

