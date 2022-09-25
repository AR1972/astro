;
PUBLIC statistics1
PUBLIC statistics2
PUBLIC statistics3
PUBLIC statistics4
PUBLIC statistics5
PUBLIC msg_in_dos_box
PUBLIC msg_dos_access2
PUBLIC msg_too_large
PUBLIC msg_xms_memory_error
PUBLIC msg_version_fail
PUBLIC msg_no_himem
PUBLIC msg_dos_access
PUBLIC magicdrvstring
PUBLIC msg_cannot_cache
PUBLIC help_text
PUBLIC whatsanasterisk
PUBLIC bufferstring1
PUBLIC bufferstring2
PUBLIC bufferstring3
PUBLIC extrastatus1
PUBLIC extrastatus2
PUBLIC extrastatus3
PUBLIC extrastatus4
PUBLIC extrastatus5
PUBLIC drivestring1
PUBLIC drivestring2
PUBLIC drivestring3
PUBLIC msg_installed

zseg    segment public 'CODE'

	assume  cs:zseg


msg_installed         db 13,10,'The memory-resident portion of SMARTDrive is loaded.',13,10,'$'
msg_in_dos_box        db 13,10,'You cannot load SMARTDrive when a shell program (such as'
                      db 13,10,'MS-DOS shell) or Windows is running. Quit Windows or the'
                      db 13,10,'shell program, and then load SMARTDrive.',13,10,'$'
msg_version_fail      db 13,10,'SMARTDrive requires MS-DOS version 3.1 or later.',13,10,'$'
msg_xms_memory_error  db 'There is insufficient XMS memory to load SMARTDrive.',13,10,'$'
msg_no_himem          db 'SMARTDrive cannot be loaded because the XMS driver, HIMEM.SYS'
                      db 13,10,'is not loaded.  Check the CONFIG.SYS file for a device=himem.sys'
                      db 13,10,'command line.',13,10,'$'
msg_cannot_cache      db 13,10,'Warning: Unable to use a disk cache on the specified drive',13,10,'$'
msg_too_large         db 13,10,'SMARTDrive configuration is too large.',13,10,'$'
msg_dos_access        db 13,10,'Non-fatal error detected: error #','$'
msg_dos_access2       db 13,10,'$'
help_text             db 13,10,'Installs and configures the SMARTDrive disk-caching utility.',13,10
                      db 13,10,'SMARTDRV [[drive[+|-]]...] [/E:ElementSize] [InitCacheSize][WinCacheSize]]'
                      db 13,10,'         [/B:BufferSize] [/C] [/R] [/L] [/Q] [/S] [/V]',13,10
                      db 13,10,'  drive           Specifies the drive for which to control caching. You can'
                      db 13,10,'                  specify multiple drives.'
                      db 13,10,'  +               Enables write-behind caching for the specified drive.'
                      db 13,10,'  -               Disables all caching for the specified drive.'
                      db 13,10,'  /E:ElementSize  Specifies how many bytes of information to move at one time.'
                      db 13,10,'  InitCacheSize   Specifies how many KB of XMS memory to use for the cache.'
                      db 13,10,'  WinCacheSize    Specifies how many KB of XMS memory to use for the cache'
                      db 13,10,'                  when running Windows.'
                      db 13,10,'  /B:BufferSize   Specifies the size of the read-ahead buffer.'
                      db 13,10,'  /C              Writes all write-behind information to the hard disk.'
                      db 13,10,'  /R              Clears the contents of the cache and restarts SMARTDrive.'
                      db 13,10,'  /L              Loads SMARTDrive into conventional memory.'
                      db 13,10,'  /Q              Prevents SMARTDrive from displaying status messages'
                      db 13,10,'                  when it starts.'
                      db 13,10,'  /S              Displays additional information about SMARTDrive',027h,'s status.'
                      db 13,10,'  /V              Displays SMARTDrive status messages when starting.',13,10,13,10,'$'
statistics1           db 'Microsoft SMARTDrive Disk Cache version 4.1'
                      db 13,10,'Copyright 1991,1993 Microsoft Corp.',13,10,'$'
statistics2           db 13,10,'Cache size: ','$'
statistics3           db ' bytes',13,10,'Cache size while running Windows: ','$'
statistics4           db ' bytes',13,10
                      db 13,10,'            Disk Caching Status'
                      db 13,10,'drive   read cache   write cache   buffering'
                      db 13,10,'--------------------------------------------','$'
statistics5           db 13,10,13,10,'For help, type "Smartdrv /?".',13,10,'$'
drivestring1          db 13,10,'  ?:       yes           yes','$'
drivestring2          db 13,10,'  ?:       yes           no ','$'
drivestring3          db 13,10,'  ?:       no            no ','$'
extrastatus1          db 13,10,'Room for ','$'
extrastatus2          db ' elements of ','$'
extrastatus3          db ' bytes each',13,10,'There have been ','$'
extrastatus4          db ' cache hits',13,10,'    and ','$'
extrastatus5          db ' cache misses',13,10,'$'
bufferstring1         db '          yes','$'
bufferstring2         db '          no','$'
bufferstring3         db '          -','$'
magicdrvstring        db 'You must specify the host drive for a DoubleSpace drive.',13,10,13,10,7,'$'
whatsanasterisk       db 13,10,13,10,'* DoubleSpace drive cached via host drive.','$'

zseg ends

end
