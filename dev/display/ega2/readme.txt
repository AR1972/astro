
MS-DOS 6 National Language Support Update - Supplement to the OAK
-----------------------------------------------------------------

The following files where included on the MS-DOS 6 Supplemental disk
as updated or alternate files for adding additional language support
for MS-DOS:

EGA.CPI
KEYB.COM
KEYBOARD.SYS

The following directories contain the source for those files:

oak\cmd\keyb\keyb2
oak\dev\keyboard\kbd2
oak\dev\display\ega2

The MAKEFILEs for these tools will not be called during a complete
build of the OAK unless the parent makefiles are modified to visit
these new directories.  These makefiles will deposit the completed
file in the BINARIES and COMPRESS directories of the OAK under the
following names:

KEYBOARD.SYS - KEYBRD2.SYS / KEYBRD2.SY_
KEYB.COM     - KEYB2.COM / KEYB2.CO_
EGA.CPI      - EGA2.CPI / EGA2.CP_

The batch file that moves these files from the OAK to the ODK in
preparation for product disk image creation will also need to be
modified.


The changes to these files are outlined below:

EGA.CPI
-------
Corrects Cyrillic display.

Note:   The EGA.CPI file built from these updated sources is suitable
as a replacement for the EGA.CPI file included in MS-DOS. The EGA.ICE
found on the MS-DOS 6 product diskettes is still required if
Icelandic support is used.


KEYB.COM
--------
Support for Canadian switchable keyboard.

Note:  This KEYB.COM is suitable as a replacement for the one
in MS-DOS. The changes to this file are for the dual layout
French-Canadian keyboard, but all other functionality remains the
same.


KEYBOARD.SYS
------------
 1)  Added keybard support for:
   - CF Canadian standard CAN/CSA-Z243.200-92
   - BR secondary Brazilian standard keyboard
       (supports keyboard IDs 274 and 275,
        with 274 as default)
   - RO Romania

 2)  Changed:
   - GR German shift lock behavior of top row:
        now remains unaffected, functionally the same as US
        (ie, "Typewriter mode" has been removed.)

Note:  The supplemental disk KEYBOARD.SYS is NOT a replacement for
the existing KEYBOARD.SYS. It is best decribed as an 'alternate' file
since it includes Icelandic, Brazillian, Romainian, and dual
French-Canadian keyboards, but is missing the following support:

  Swiss German     (keyb sg)
  Swiss French     (keyb sf)
  Japan            (keyb jp)

In addition, the following keyboards are now accessible by Letter
only (previously could be accessed by both letter and number):  Pl,
Cz, Sl, Yu, Hu.  The removal of the above support was due to size
limitations.

Additional Notes for Icelandic support:  The COUNTRY.ICE and
KEYBOARD.ICE files for Icelandic are superceded by the COUNTRY.SYS
and the KEYBOARD.SYS files on the new supplemental disk, but the
EGA.ICE file will still be required for Icelandic code page support.
EGA.CPI is sufficient for all other supported languages.



Source file differences
-----------------------
Below is a summary of the files that changed between the original
OAK source files and the updates included in this supplement.  Note
that all source files for each utility are included, even if they are
unchanged. 


CMD\KEYB\KEYB2
--------------
The following file has been added:
KEYBSHAR.INC - normally in OAK\INC directory

The following source files have been changed:
KEYBCMD.ASM
KEYBI9.ASM
KEYBI9.INC
KEYBI9C.ASM
KEYBI9C.INC
MAKEFILE


DEV\KEYBOARD\KBD2
-----------------
The following files have been added:
KEYBSHAR.INC - normally in OAK\INC directory
KDFBR2.ASM
KDFCP.ASM
KDFIC.ASM
KDFRO.ASM

The following source files have been changed:
KDFCF.ASM
KDFGE.ASM
KDFNOW.ASM
KEYBMAC.INC
KEYBOARD.LNK
MAKEFILE


DEV\DISPLAY\EGA2
----------------
The following source files have been changed:
852-8X14.ASM
852-8X16.ASM
852-8X8.ASM
MAKEFILE
