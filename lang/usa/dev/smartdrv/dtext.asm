;
PUBLIC warning1
PUBLIC warning2
PUBLIC noloadumb
PUBLIC signon
PUBLIC signoff

vseg segment byte public 'CODE'
	assume cs:vseg,ds:vseg,es:NOTHING

signon      db 13,10,'Smartdrv double buffering manager installed.',13,10,'$'
signoff     db 13,10,'Smartdrv double buffering manager not installed.',13,10,'$'
warning1    db 13,10,020h,'$'
warning2    db 020h,'$'
noloadumb   db 13,10,'Double-buffering driver cannot be loaded into a UMB.'
            db 13,10,'Do not use the devicehigh command or other load-high'
            db 13,10,'utilities to load SMARTDRV.EXE.',13,10,7,7,7,7,'$'

vseg ends
end
