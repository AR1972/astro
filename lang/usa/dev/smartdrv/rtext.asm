;
PUBLIC popup1
PUBLIC popup2

zseg    segment public 'CODE'

	assume  cs:zseg
	assume	ds:nothing

popup1 db 0DAh
       db 37 dup(0C4h)
       db 0BFh,0,0B3h
       db 37 dup(020h)
       db 0B3h,0,0B3h,020h
       db 'ATTENTION: A serious disk error has'
       db 020h,0B3h,0,0B3h,020h
       db 'occured while writing to drive @.'
       db 020h,020h,020h,0B3h,0,0B3h,020h
       db 'Retry (r)?&_'
       db 24 dup(020h)
       db 0B3h,0,0B3h
       db 37 dup(020h)
       db 0B3h,0,0C0h
       db 37 dup(0C4h)
       db 0D9h,020h,'$'
popup2 db 0DAh
       db 29 dup(0C4h)
       db 0BFh,0,0B3h
       db 29 dup(020h)
       db 0B3h,0,0B3h,020h
       db 'Waiting for system shutdown.'
       db 0B3h,0,0B3h
       db 29 dup(020h)
       db 0B3h,0,0C0h
       db 29 dup(0C4h)
       db 0D9h,'$'

zseg ends

end
