
bins    =..\..\binaries
cprs    =..\..\compress

!IFNDEF       BUILDER

all: mouse.com

!ELSE

all: mouse.com $(bins)\mouse.com $(cprs)\mouse.co_


$(bins)\mouse.com: mouse.com
                    copy mouse.com $(bins)

mouse.co_:         mouse.com
                    $(compress) -f mouse.com mouse.co_

$(cprs)\mouse.co_: mouse.co_
                    copy mouse.co_ $(cprs)

!ENDIF

clean:
    del *.co_

