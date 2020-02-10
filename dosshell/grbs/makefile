#*************************** Makefile for DOS ***************************

bins    =..\..\binaries
cprs    =..\..\compress


#
#######################	dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: mono.grb

!ELSE

all: mono.grb                                 \
     $(bins)\mono.grb     $(cprs)\mono.gr_    \
     $(bins)\cga.grb      $(cprs)\cga.gr_     \
     $(bins)\ega.grb      $(cprs)\ega.gr_     \
     $(bins)\vga.grb      $(cprs)\vga.gr_     \
     $(bins)\egamono.grb  $(cprs)\egamono.gr_ \
     $(bins)\vgamono.grb  $(cprs)\vgamono.gr_ \
     $(bins)\herc.grb     $(cprs)\herc.gr_


$(bins)\mono.grb: mono.grb
              copy mono.grb $(bins)

mono.gr_:       mono.grb
              $(compress) mono.grb mono.gr_

$(cprs)\mono.gr_:  mono.gr_
              copy mono.gr_ $(cprs)


$(bins)\cga.grb: cga.grb
              copy cga.grb $(bins)

cga.gr_:       cga.grb
              $(compress) cga.grb cga.gr_

$(cprs)\cga.gr_:  cga.gr_
              copy cga.gr_ $(cprs)


$(bins)\ega.grb: ega.grb
              copy ega.grb $(bins)

ega.gr_:       ega.grb
              $(compress) ega.grb ega.gr_

$(cprs)\ega.gr_:  ega.gr_
              copy ega.gr_ $(cprs)


$(bins)\egamono.grb: egamono.grb
              copy egamono.grb $(bins)

egamono.gr_:       egamono.grb
              $(compress) egamono.grb egamono.gr_

$(cprs)\egamono.gr_:  egamono.gr_
              copy egamono.gr_ $(cprs)


$(bins)\vga.grb: vga.grb
              copy vga.grb $(bins)

vga.gr_:       vga.grb
              $(compress) vga.grb vga.gr_

$(cprs)\vga.gr_:  vga.gr_
              copy vga.gr_ $(cprs)


$(bins)\vgamono.grb: vgamono.grb
              copy vgamono.grb $(bins)

vgamono.gr_:       vgamono.grb
              $(compress) vgamono.grb vgamono.gr_

$(cprs)\vgamono.gr_:  vgamono.gr_
              copy vgamono.gr_ $(cprs)


$(bins)\herc.grb: herc.grb
              copy herc.grb $(bins)

herc.gr_:       herc.grb
              $(compress) herc.grb herc.gr_

$(cprs)\herc.gr_:  herc.gr_
              copy herc.gr_ $(cprs)


!ENDIF

mono.obj:    mono.asm


mono.grb:     mono.obj
              link mono;
              reloc mono mono.grb
              del mono.exe
