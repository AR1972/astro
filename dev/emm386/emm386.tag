
M000 01/09/91 HKN pagetrap.asm	clear int flag at page fault time and restore
				it in trapwrites. Also clear the Trap flag on
				on the stack in trapwrites before restoring 
				it from PFWflags.

M001 01/14/91 HKN emm40.asm	In performdatamove fix up pointers if the 
				direction of the move is reversed after 
				completing the dword moves.

M002 01/18/91 HKN winsrch.asm	use xchg instead of move to write into 
				adapter space for ram determination.

M003 01/21/91 HKN ps2table.asm  Added an entry for the IBM Artic Adapter
				(@EFF0.ADF). 

M004 01/30/91 HKN winsrch.asm   When trying to reclaim shadow ROM in routine
				CheckForCompaqROM, check to make sure that
				the offset of the int 10 vector lies within
				ROM length as specifed in the ROM header.

M005 01/30/91 HKN xms.asm	Hook QueryA20 (function 07h) also.

M006 02/08/91 HKN dmatrap.asm	Initialize the channel 4 data structure for
				MCA machines. Also make DMAMask/UnMaskChannel
				sensitive to programming of the extended MCA
				ports.

M007 02/08/91 HKN dmatrap.asm   Use the right equate when initializing the 
				DMA count info for EISA machines.


M008 02/13/91 HKN init.asm	detect a Compaq 386/16 DeskPro or a Compaq
				portable 386.
		  memm386.asm   Do not make int 16s on a Compaq Deskpro
		  util.asm 	386/16 or a Compaq portable 386.
		  emm386.inc	Added fCPQ16 & fCPQ16Bit

M009 02/14/91 SMR xms.asm	B#5875. Forced EMM386 to use XMS to control
				A20 line, instead of h/w method.

M010 02/14/91 HKN init.asm	B#5792. added Y= parameter for path 
		  winemm.asm    specification for loadhi Vxd



M011 02/21/91 HKN iotrap.asm	B#5483. do not simulate IO address wrap.

M012 02/27/91 HKN dmatrap.asm   B#6097. use ah to test for LONG in int 13 
				handler.

M013 02/27/91 HKN winemm.asm	First Check if standard mode has issued the 
				int 2f startup in rint2fhHandler. If so just 
				chain.

M014 03/06/91 HKN int15.asm	Restore cx before checking to see if it is 0.
				
M015 03/07/91 HKN winemm.asm	Clear out the high word of edx when 
				initializing dx with the arena length in 
				VxdUsedUMBList.

