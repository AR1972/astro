extcsw  =-Osleazrgi -Gs -Zl -Gr -AS -NT FX_TEXT
extasw  =/Cp /Dmodel=MEDIUM /Dtext=FX_TEXT

.asm.obj:
                ml /c /nologo $(awarn) $(extasw) $*.asm

all:    fastlynx.lib

clonesub.obj:   clonesub.asm            \
                fastlynx.inc            \
                macros.inc              \
                model.inc               \
                makefile

crctab.obj:     crctab.asm              \
                fastlynx.inc            \
                macros.inc              \
                model.inc               \
                makefile

init.obj:       init.asm                \
                fastlynx.inc            \
                macros.inc              \
                model.inc               \
                makefile

listen.obj:     listen.asm              \
                fastlynx.inc            \
                macros.inc              \
                model.inc               \
                makefile

loader.obj:     loader.asm              \
                model.inc               \
                makefile

parallel.obj:   parallel.asm            \
                fastlynx.inc            \
                macros.inc              \
                model.inc               \
                makefile

querybio.obj:   querybio.asm            \
                fastlynx.inc            \
                macros.inc              \
                model.inc               \
                makefile

saveport.obj:   saveport.asm            \
                fastlynx.inc            \
                macros.inc              \
                model.inc               \
                makefile

sendrecv.obj:   sendrecv.asm            \
                fastlynx.inc            \
                macros.inc              \
                model.inc               \
                makefile

serial.obj:     serial.asm              \
                fastlynx.inc            \
                macros.inc              \
                model.inc               \
                makefile

clone.obj:      clone.c                 \
                fastlynx.h              \
                makefile

fastlynx.lib:   parallel.obj            \
                clonesub.obj            \
                listen.obj              \
                init.obj                \
                crctab.obj              \
                querybio.obj            \
                sendrecv.obj            \
                saveport.obj            \
                serial.obj              \
                loader.obj              \
                clone.obj               \
                fastlynx.lrf
                del fastlynx.lib
                lib @fastlynx.lrf
