#************************** makefile for cmd\... ***************************

bins    =..\..\binaries
cprs    =..\..\compress
dest1   =netwksta.2xe
cpr1    =netwksta.2x_
dest2   =redir.2xe
cpr2    =redir.2x_
dest3   =setname.exe
cpr3    =setname.ex_
dest4   =net5.com
cpr4    =net5.co_
dest5   =DMDRVR.BIN
cpr5    =DMDRVR.BI_
dest6   =IPX.OBJ
cpr6    =IPX.OB_
dest7   =NETBEUI.DOS
cpr7    =NETBEUI.DO_
dest8   =NETWKSTA.1XE
cpr8    =NETWKSTA.1X_
dest9   =REDIR.1XE
cpr9    =REDIR.1X_
dest10  =SSTOR.SYS
cpr10   =SSTOR.SY_
dest11  =TBMI2.COM
cpr11   =TBMI2.CO_
dest12  =XBIOS.OVL
cpr12   =XBIOS.OV_
dest13  =net.exe
cpr13   =net.ex_

#
#######################       dependencies begin here. #########################
#

!IFNDEF       BUILDER

all: $(dest1) $(dest2) $(dest3) $(dest4)

!ELSE

all: $(dest5)  $(bins)\$(dest5)  $(cprs)\$(cpr5) \
     $(dest10) $(bins)\$(dest10) $(cprs)\$(cpr10) \
     $(dest12) $(bins)\$(dest12) $(cprs)\$(cpr12) \

$(bins)\$(dest1): $(dest1)
          copy $(dest1) $(bins)

$(cprs)\$(cpr1):  $(cpr1)
          copy $(cpr1) $(cprs)

$(cpr1):       $(dest1)
          $(compress) $(dest1) $(cpr1)

$(bins)\$(dest2): $(dest2)
          copy $(dest2) $(bins)

$(cprs)\$(cpr2):  $(cpr2)
          copy $(cpr2) $(cprs)

$(cpr2):       $(dest2)
          $(compress) $(dest2) $(cpr2)

$(bins)\$(dest3): $(dest3)
            copy $(dest3) $(bins)

$(cprs)\$(cpr3):  $(cpr3)
            copy $(cpr3) $(cprs)

$(cpr3):       $(dest3)
            $(compress) $(dest3) $(cpr3)

$(bins)\$(dest4): $(dest4)
            copy $(dest4) $(bins)

$(cprs)\$(cpr4):  $(cpr4)
            copy $(cpr4) $(cprs)

$(cpr4):       $(dest4)
            $(compress) $(dest4) $(cpr4)

$(bins)\$(dest5): $(dest5)
            copy $(dest5) $(bins)

$(cprs)\$(cpr5):  $(cpr5)
            copy $(cpr5) $(cprs)

$(cpr5):       $(dest5)
            $(compress) $(dest5) $(cpr5)

$(bins)\$(dest6): $(dest6)
            copy $(dest6) $(bins)

$(cprs)\$(cpr6):  $(cpr6)
            copy $(cpr6) $(cprs)

$(cpr6):       $(dest6)
            $(compress) $(dest6) $(cpr6)

$(bins)\$(dest7): $(dest7)
            copy $(dest7) $(bins)

$(cprs)\$(cpr7):  $(cpr7)
            copy $(cpr7) $(cprs)

$(cpr7):       $(dest7)
            $(compress) $(dest7) $(cpr7)

$(bins)\$(dest8): $(dest8)
            copy $(dest8) $(bins)

$(cprs)\$(cpr8):  $(cpr8)
            copy $(cpr8) $(cprs)

$(cpr8):       $(dest8)
            $(compress) $(dest8) $(cpr8)

$(bins)\$(dest9): $(dest9)
            copy $(dest9) $(bins)

$(cprs)\$(cpr9):  $(cpr9)
            copy $(cpr9) $(cprs)

$(cpr9):       $(dest9)
            $(compress) $(dest9) $(cpr9)

$(bins)\$(dest10): $(dest10)
            copy $(dest10) $(bins)

$(cprs)\$(cpr10):  $(cpr10)
            copy $(cpr10) $(cprs)

$(cpr10):       $(dest10)
            $(compress) $(dest10) $(cpr10)

$(bins)\$(dest11): $(dest11)
            copy $(dest11) $(bins)

$(cprs)\$(cpr11):  $(cpr11)
            copy $(cpr11) $(cprs)

$(cpr11):       $(dest11)
            $(compress) $(dest11) $(cpr11)

$(bins)\$(dest12): $(dest12)
            copy $(dest12) $(bins)

$(cprs)\$(cpr12):  $(cpr12)
            copy $(cpr12) $(cprs)

$(cpr12):       $(dest12)
            $(compress) $(dest12) $(cpr12)

$(bins)\$(dest13): $(dest13)
            copy $(dest13) $(bins)

$(cprs)\$(cpr13):  $(cpr13)
            copy $(cpr13) $(cprs)

$(cpr13):       $(dest13)
            $(compress) $(dest13) $(cpr13)

!ENDIF
