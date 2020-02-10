Files copied from other places:
===============================

CMACROS.INC	: generated in \lib\inc
STD.INC		: generated in \lib\inc
VKEY.INC	: generated in \lib\cw\inc
INDRV.INC	: generated in \lib\cw\inc
INSCR.INC	: generated in \lib\cw\inc (should be called INCSD.INC)
INKBD.INC	: generated in \lib\cw\inc
INSYD.INC	: generated in \lib\cw\inc
INMOU.INC	: generated in \lib\cw\inc
INGXD.INC	: generated in \lib\cw\inc

Note: these files are copied here in order for the DRV project to build
outside the rest of the LIB directory tree (since we export parts of the DRV
tree outside of Microsoft).

Changes in these files must be scrutinized (especially IN*.INC) since
incorrect changes may not be backwardly compatible with existing drivers.

Problems or questions -- see ScottRa.
