bins    =..\..\binaries
cprs    =..\..\compress


########################### International Changes #######################

MSG_DIR=$(LANG_SRC)\$(COUNTRY)\dosshell\inis

#########################################################################



ind1    =$(MSG_DIR)\cga.ini
inc1    =cga.in_
ind2    =$(MSG_DIR)\ega.ini
inc2    =ega.in_
ind3    =$(MSG_DIR)\mono.ini
inc3    =mono.in_
!IF "$(COUNTRY)" == "JAPAN" || "$(COUNTRY)" == "japan"
ind4    =jega.ini
inc4    =jega.in_
!ENDIF

!IFNDEF       BUILDER

all:


!ELSE

all: $(bins)\$(ind1) $(cprs)\$(inc1) \
     $(bins)\$(ind2) $(cprs)\$(inc2) \
!IF "$(COUNTRY)" == "JAPAN" || "$(COUNTRY)" == "japan"
     $(bins)\$(ind3) $(cprs)\$(inc3) \
     $(bins)\$(ind4) $(cprs)\$(inc4)
!ELSE
     $(bins)\$(ind3) $(cprs)\$(inc3)
!ENDIF

$(bins)\$(ind1): $(ind1)
            copy $(ind1) $(bins)

$(cprs)\$(inc1):  $(inc1)
            copy $(inc1) $(cprs)

$(inc1):       $(ind1)
            $(compress) $(ind1) $(inc1)


$(bins)\$(ind2): $(ind2)
            copy $(ind2) $(bins)

$(cprs)\$(inc2):  $(inc2)
            copy $(inc2) $(cprs)

$(inc2):       $(ind2)
            $(compress) $(ind2) $(inc2)


$(bins)\$(ind3): $(ind3)
            copy $(ind3) $(bins)

$(cprs)\$(inc3):  $(inc3)
            copy $(inc3) $(cprs)

$(inc3):       $(ind3)
            $(compress) $(ind3) $(inc3)


!IF "$(COUNTRY)" == "JAPAN" || "$(COUNTRY)" == "japan"
$(bins)\$(ind4): $(ind4)
            copy $(ind4) $(bins)

$(cprs)\$(inc4):  $(inc4)
            copy $(inc4) $(cprs)

$(inc4):       $(ind4)
            $(compress) $(ind4) $(inc4)
!ENDIF

!ENDIF
