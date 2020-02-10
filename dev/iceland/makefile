#*************************** Makefile for DOS ***************************

bins    =..\..\binaries
cprs    =..\..\compress


#
#######################	dependencies begin here. #########################
#

!IFNDEF       BUILDER

all:

!ELSE

all: $(bins)\country.ice  $(cprs)\country.ic_  \
     $(bins)\ega.ice      $(cprs)\ega.ic_      \
     $(bins)\keyboard.ice $(cprs)\keyboard.ic_ \
     

$(bins)\country.ice: country.ice
                    copy country.ice $(bins)

$(cprs)\country.ic_: country.ice
                    compress -f country.ice $(cprs)


$(bins)\ega.ice: ega.ice
                    copy ega.ice $(bins)

$(cprs)\ega.ic_: ega.ice
                    compress -f ega.ice $(cprs)


$(bins)\keyboard.ice: keyboard.ice
                    copy keyboard.ice $(bins)

$(cprs)\keyboard.ic_: keyboard.ice
                    compress -f keyboard.ice $(cprs)


!ENDIF
