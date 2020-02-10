bins    =..\..\binaries
cprs    =..\..\compress
vin1    =cgasnow.vid
vid1    =cga.vid
vic1    =cga.vi_
vin2    =ega.vid
vid2    =ega.vid
vic2    =ega.vi_
vin3    =herc102.vid
vid3    =herc.vid
vic3    =herc.vi_
vin4    =vga.vid
vid4    =vga.vid
vic4    =vga.vi_
!IF "$(COUNTRY)" == "JAPAN" || "$(COUNTRY)" == "japan"
vin5    =jega.vid
vid5    =jega.vid
vic5    =jega.vi_
!ENDIF
vin6    =ibm8514.vid
vid6    =8514.vid
vic6    =8514.vi_

!IFNDEF       BUILDER

all: ..\dosshell.vid

..\dosshell.vid: vga.vid
                copy vga.vid ..\dosshell.vid

!ELSE

all: $(bins)\$(vid1) $(cprs)\$(vic1) \
     $(bins)\$(vid2) $(cprs)\$(vic2) \
     $(bins)\$(vid3) $(cprs)\$(vic3) \
!IF "$(COUNTRY)" == "JAPAN" || "$(COUNTRY)" == "japan"
     $(bins)\$(vid4) $(cprs)\$(vic4) \
     $(bins)\$(vid5) $(cprs)\$(vic5) \
!ELSE
     $(bins)\$(vid4) $(cprs)\$(vic4) \
!ENDIF
     $(bins)\$(vid6) $(cprs)\$(vic6) 


$(bins)\$(vid1): $(vin1)
            copy $(vin1) $(bins)\$(vid1)

$(cprs)\$(vic1):  $(vic1)
            copy $(vic1) $(cprs)

$(vic1):       $(vin1)
            $(compress) $(vin1) $(vic1)


$(bins)\$(vid2): $(vin2)
            copy $(vin2) $(bins)\$(vid2)

$(cprs)\$(vic2):  $(vic2)
            copy $(vic2) $(cprs)

$(vic2):       $(vin2)
            $(compress) $(vin2) $(vic2)


$(bins)\$(vid3): $(vin3)
            copy $(vin3) $(bins)\$(vid3)

$(cprs)\$(vic3):  $(vic3)
            copy $(vic3) $(cprs)

$(vic3):       $(vin3)
            $(compress) $(vin3) $(vic3)


$(bins)\$(vid4): $(vin4)
            copy $(vin4) $(bins)\$(vid4)

$(cprs)\$(vic4):  $(vic4)
            copy $(vic4) $(cprs)

$(vic4):       $(vin4)
            $(compress) $(vin4) $(vic4)

!IF "$(COUNTRY)" == "JAPAN" || "$(COUNTRY)" == "japan"
$(bins)\$(vid5): $(vin5)
            copy $(vin5) $(bins)\$(vid5)

$(cprs)\$(vic5):  $(vic5)
            copy $(vic5) $(cprs)

$(vic5):       $(vin5)
            $(compress) $(vin5) $(vic5)
!ENDIF
$(bins)\$(vid6): $(vin6)
            copy $(vin6) $(bins)\$(vid6)

$(cprs)\$(vic6):  $(vic6)
            copy $(vic6) $(cprs)

$(vic6):       $(vin6)
            $(compress) $(vin6) $(vic6)


!ENDIF

