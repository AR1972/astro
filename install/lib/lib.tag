M000 01/12/90 JAH    all files    Converted to allow building as either
                                  COMPACT or LARGE memory model.

M001 02/23/91 JAH    drv_type.asm	Change cbw to xor AH,AH to avoid
					extending sign bit on return value.


M100 02/16/91 JAH    video.asm    Added min checking for video initialization
                                  and fixed CGA/MDA/HERC detection.

m112 08/10/91 JAH    video.asm    Added detection for ATI WONDER and VIP
                                  cards to disable scanline/font change


m114 08/22/91 JAH    video.asm    Changed offset of ATI ROM signature from
                                  30 to 31.

m119 09/06/91 DLB    video.asm	  Change Herc delay from 0FFFh to 6FFFh for
				  proper detection of Twinhead Herc adapters.

I002 10/01/91 JAH    video.asm    Implmented IBM fix for CGA monitor on
                                  on EGA adapter.

M125 10/02/91 JAH    drv_type.asm Fixed problem when a 2.88 meg drive type
				  is returned by DOS to GetDriveType();
				  Bug #2823.
