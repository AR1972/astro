
SUBDIR	       		WHAT IT CONTAINS
------	       		----------------
STUB		 (1)	SRC for LHITST.ASM a dummy stub which
	       		traps the WIN386 startup call and 
	       	        loads the VXD (LOADHI.EXE). As an
	       		example it allocates a hunk of XMS
	       		memory and pretends that there are
	       		16 UMB pages starting at 0C800:0

		 (2)    MAKESTUB.BAT
			This assembles and links the test stub
			and copies over the exe to the root.
	       		
	       		The Limulator should take the place
	       		of this dumb stub.

                 (3)    INT2FAPI.INC -- stub code include file


SRC		 (1)	SRC for UMB.ASM. This file provides 
	       		the interface between the Limulator
	       		and the VXD and is a part of the VXD.
	       		
	       		Change this file to match the
	       		the Limulators interface.

                  (2)   SRC for COPYINST.ASM. This file has a routine
			that is not a part of the VxD itself,
			however, this routine should be called by the
			LIMulator at "virtual-disable" time to
			extract and copy out UMB related instance data 
		        information from the Windows/386 3.00 initialization
			data structure into a table inside the UMB.
			The address of this table is later supplied
			to the VxD via one of the routines in UMB.ASM 

	       	  (3)	LOADHI.DEF
	       		Change the STUB statement in this
	       		file appropriately (should be the
	       		LIMulator EXE)

		  (4)   MAKEUMB.BAT assembles UMB.ASM and copies the
			obj over to the RETOBJ & DBGOBJ directories
			(the sample UMB.ASM does not have any debug
			 code).

INCLUDE           (1)   VMM.INC needed to build UMB.ASM

RETOBJ		  Retail version of the fixed obj files.
		
		  (1)	LOADHI.OBJ, INSTINIT.OBJ, INSTSWAP.OBJ
			These are precompiled retail OBJs used during link

		  (2)   UMB.OBJ will be copied over here after 
			assembling UMB.ASM in the SRC subdir

DBGOBJ		  Debug version of the fixed OBJ files.

		  (1)	LOADHI.OBJ, INSTINIT.OBJ, INSTSWAP.OBJ
			These are precompiled debug OBJs used during link

		  (2)   UMB.OBJ will be copied over here after 
			assembling UMB.ASM in the SRC subdir
	
TOOLS			MASM5.EXE, LINK5.EXE, LINK386.EXE, ADDHDR.EXE
			
			These are all the tools that will be needed
			to build the VXD.


ROOT directory		WHAT IT CONTAINS
--------------		----------------

		  (1)	LOADHID.LNK	--  used in linking debug VXD
		  (2)	LOADHIR.LNK	--  used in linking retail VXD
		  (3)   MAKEVXDD.BAT	--  builds the debug VXD.
		  (4)   MAKEVXDR.BAT	--  builds the retail VXD.
                  (5)   README.TXT      --  this readme file


STEPS:

	(1) go to the STUB directory and run MAKESTUB.BAT

	    LHITST.EXE would be built and copied over to the root.

        (2) go to the SRC directory and run MAKEUMB.BAT
	
	    UMB.OBJ would be built and copied over to the OBJ dir

        (3) go to the root and run MAKEVXDD.BAT	or MAKEVXDR.BAT
	    to build the debug or retail versions respectively.

	    builds LOADHI.EXE which is the STUB combimed with the
	    VXD


NOTE:

	(1) If your LIMulator device drivers has an .EXE format:

	    LHITST.EXE is a dummy stub. In the final product, your
	    LIMulator EXE file would be the stub. The VxD would be
	    appended as an extended part in the .EXE file.

            In this case, you will end up having only one file which
	    will hold both the LIMulator device driver as well as the
	    LoadHi VxD

        (2) If your LIMulator device driver is a .COM format file

	    In this case, the VxD and the device driver cannot be 
	    clubbed together. You will have to modify the .DEF file
	    to take away the 'stub' statement and modify the
	    MAKEVXD?.BAT file to avoid doing the 'ADDHDR' part.

            You end up having a separate VxD file. However, at
	    Windows load time it is the responsibility of the
	    LIMulator VxD to provide the complete path name of the
	    LoadHi VxD file.

