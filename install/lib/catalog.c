/****************************************************************************/
	int AbsReadWrite( int Drive, struct ABSIO_PACKET far *absPack,
	  		int ReadWrite );

	INCLUDE: DISK_IO.H

	LOCATION: \DISK\ABS_RDWR.ASM


	ARGUMENTS: 
			absPack - Ptr to DOS 4.x int 25,26 access packet
			Drive	- Drive number (A=0,B=1,C=2,...)
			ReadWrite-	Flags whether to do a read or write
		   		  		operation: 0  = READ, !0 = WRITE
	RETURNS:	
			int - 0 if successful else error code

	Does an absolute sector read or write using int
	25h or 26h. Special care is taken to be sure the
	function will work under any version of DOS
	2.0 - 4.x.
/****************************************************************************/




/****************************************************************************/
	void AuxPuts( char *Str )

	INLCUDE: BIOS_IO.H

	LOCATION: \BIOS\AUXPUTS.ASM
	

	ARGUMENTS: Str	- Ptr to string to display
	RETURNS:   void

	Outputs a string to the STDAUX device. (com1)

/****************************************************************************/





/***************************************************************************/
	int BigReadWrite( int iFile, char far *Buf, long lBytes, int RdWr )

	INCLUDE: FILE_IO.H

	LOCATION: \FILE\F_RDWR.C	
																						
	ARGUMENTS:	iFile - Open DOS file handle, file pointer must be set	
							 	to position for start of read or write				
					*Buf - buffer to read to or write from
					lBytes - Number of bytes to read or write						
					fRdWr - Flags 0 for read and 1 for write						
	RETURN:		int 	- OK if no error - ERROR if read or write error		
																								

	Reads or writes a requested number of bytes into a specified buffer. 	
	The number of bytes requested may be > 64K without causing a problem.	

/***************************************************************************/




/***************************************************************************/
	BuildDosPartEntry( struct PartMap *Map, struct HdParms *Parms,				
					 	struct Part *PartEntry )											
																								

	INCLUDE: HDISK.H

	LOCATION: \HDISK\MAKEPART.C

						
	ARGUMENTS:	Map		- Ptr partition map entry									
					Parms		- Ptr to an initialize hard disk parameters struct	
					PartEntry- Ptr to partitin entry to be filled in				
	RETURNS:		void																			
				  

	Builds a DOS paritition entry from a partition map entry. Determines 	
	the type of DOS partition needed based on the number of sectors in the	
	partition area. To be compatible with all previous versions of DOS the	
	partition will start the first sector of a cylinder and end on the last 
	sector of a cylinder.																	
																				
	NOTE: Must remember that heads and cylinders are 0 based while sectors	
			are 1 based.																		
																								
	Head = ((AbsSector % SectorPerTrack) * NumberOfHeads) / SectorsPerTrack	
	Track =	AbsSector / ( NumberOfHeads * SectorsPerTrack )						
	Sector = (absSector % SectoPerTrack) + 1											
																								
/***************************************************************************/




/***************************************************************************/
	void BuildPath( char *szPath, char *szDirPath, char *szFileName ) 		
																								
	INCLUDE: STRLIB.H
	
	LOCATION: \STRLIB\BLDPATH.C

	
	ARGUMENTS:	szPath	-  Buffer to hold the created pathname					
					chDrive	-	Drive designator											
					szDirPath -	Pointer to drive and directory path					
					szFileName - Pointer to a file name									
	RETURNS: 	void																			


	Builds a valid path name when given the 3 elements of drive, path and	
	file name along with a buffer for the created path.
																								
/***************************************************************************/






/***************************************************************************/
	BuildPartMap( struct Part *Part, struct PartMap *Map,							
						 	struct HdParms *Parms )										

	INCLUDE: HDISK.H

	LOCATION: \HDISK\PARTMAP.C

						 
	ARGUMENTS:	Part	- Ptr to partition entry table to build the map from	
					Map	- Ptr to array of at least 9 partition map structures	
					Parms - Ptr to hard disk parameter structure 					
	RETURNS:		void																			
																								

	Builds a map of the entire partition layout of a hard disk. The map is	
	sorted by physical starting disk sectors and contains all free and		
	allocated areas of the disk.															
	
/***************************************************************************/




/***************************************************************************/
	int CenterLength( int iStrLen )														

	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_CENTER.C
																								
	ARGUMENTS:	int iStrLen - The lenght of a string to be centered on 		
									  the current display.									
	RETURNS:		int - Starting column to make the string centered	
																								

	Function to return the starting column of a string consisting of StrLen 
	number of characters for the string to be centered on a line of the     
	current screen. The function will adjust current width of the screen.   
	
/***************************************************************************/




/***************************************************************************/
	int CenterStr( char *szString )														
				 
	INCLUDE: WINDOW.H
																			
	LOCATION: \WINDOW\W_STRCEN.C

	ARGUMENTS:	szString - pointer to a string                              

	RETURNS:	int column number which causes the string to be centered		

	Returns the display column which cause the string passed as an argument 
	to be displayed centered on the screen. 										   
/***************************************************************************/





/***************************************************************************/
	int CheckDBCSTailByte (unsigned char *str,unsigned char *point)

	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\DBCS.C


	input:	*str = strart pointer of the string
				*point = character pointer to check
	output:	TRUE if at the tail byte


	Check if the character point is at tail byte
/***************************************************************************/




/****************************************************************************/
	int CheckDmaBound( void far *Buffer, unsigned Bytes )

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DMA_CHK.ASM
	
	ARGUMENTS:	Buffer	- Ptr to memory area
					Bytes	- Lenght of memory area in byts
	RETURN:	int - OK if not on boudary else !OK

	Checks to see if an area of memory will cross a DMA
	boundary. Returns OK if not on a boundary else return
	!OK.
/****************************************************************************/




/***************************************************************************/
	int CheckForMouseDriver ( void )

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\MOUSEDRV.ASM

	EXIT: int: 1 if mouse driver is installed, 0 otherwise

	Checks if a mouse driver has been installed.
/***************************************************************************/





/***************************************************************************/
	char	ChipTechVGA(void);

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\VIDTEST.ASM


	EXIT:
     	- TRUE if C & T VGA is present, FALSE otherwise

	Returns true if we have a Chips and Technologies 441 VGA.
	Before calling this routine confirm we have a VGA from VideoAdapter()
	This routine should be used only to distinguish IBM VGA from C&T VGA.

/***************************************************************************/





/***************************************************************************/
	void CreateMapEntry( struct PartMap *Map, struct Part *Part )				
																								
	INCLUDE: HDISK.H

	LOCATION: \HDISK\MAP_ENT.C


	ARGUMENTS:	Map	-	Ptr to partition map structure							
					Part	-	Ptr to partition table entry								
	RETURNS: 	void																			
																								

	Creates a partition map entry from a partition entry structure.			

/***************************************************************************/





/***************************************************************************/
	int CreatRoot( int iDrv, struct BPB *Bpb )										
													  
	
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_ROOT.ASM
										
	ARGUMENTS:	iDrv	-	Physical drive number										
					Bpb	-	Bpb structure for disk in the drive						
	RETURNS:		int	-	OK if successful												

	Creates an empty root directory on the specified drive using the			
	specified BPB for the size and location.
/***************************************************************************/




/***************************************************************************/
	unsigned char *DBCSstrchr(unsigned char *str,unsigned char c)

	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\DBCS.C


	ENTRY: str - string to search for character
			 c -- character to search for

	EXIT:  pointer to first occurence of character if succesful, NULL otherwise


	Does a DBCS enabled strchr to find the first occurence of a character
	in a string.

/***************************************************************************/





/***************************************************************************/
	unsigned char	*DBCSstrupr(unsigned char *str)

	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\DBCS.C


	ENTRY: String to convert to upper case.

	EXIT: Pointer to string that was passed in.

	Does a DBCS enabled strupr.

/***************************************************************************/





/***************************************************************************/
	int  detect8514(void);

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\VIDTEST.ASM


	EXIT: True if 8514 display card is present and contains proper memory 
      	configuration, False otherwise.


	This function detects the presence of an 8514 display card.
	The way we do this is to first write to the Error Term Register and then
	make sure we can read back the value we wrote. Next I read in the value of
	the subsystem status register and check bit 7 to determine if the 8 plane
	memory configuration is present. Only if both these conditions are
	satisfied will we install the 8524 display driver.

/***************************************************************************/




/***************************************************************************/
	void DisplayScreenHeader( void )														
																								
	INCLUDE: BIOS_IO.H

	LOCATION: \PROMPTS\HEADER.C


	ARGUMENTS:	NONE																			

	RETURNS:		void 																			
																								
	EXTERNS:		HelpHeaderText - Both declared in EXTERN.C						
					HeaderText;																	
																								

	Displays the header and title at the top of the screen in the proper		
	color as defined by the macro GetHeaderColor().									
					
/***************************************************************************/




/***************************************************************************/
	void DisplayText( char *szText[], int StartRow  )						
		  
	INCLUDE: BIOS_IO.H
	
	LOCATION: \PROMPTS\DISPLAY.C
																				
	
	ARGUMENTS:	szText		- array of pointers to strings. Last element		
									  	should be a NULL pointer. (max 9 strings)		
					StartRow		- Screen row where text starts						
	RETURNS:		void																			
																								

	Displays multiple lines of text on the screen starting at the specified	
	row. 
	
/***************************************************************************/




/***************************************************************************/
	void DisplayTitle( char *szTitle )													
		
	INCLUDE: HDISK.H

	LOCATION: \HDISK\DISPLAY.C

																				
	ARGUMENTS:	szTitle	- Ptr to string to be displayed.							
	RETURNS:		void																			
	
	
	Displays a string centered horizonally on the screen starting at			
	the position specified by the #define TITLE_ROW.								
																				
/***************************************************************************/






/****************************************************************************/
	void DoInt2f( unsigned Percent );


	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\DOINT2F.ASM

	
	ARGUMENTS: Percent - Percentage of format complete
	
	RETURNS:   void

	Does an int 2f with AX = 4400h to update the format
	complete gage. This function is mainly for getting
	the gage setup before the format program is spawned.

/****************************************************************************/




/****************************************************************************/
	int	_dos_dskreset( void )

	LOCATION: \DISK\DSK_RSET.ASM

	RETURNS: nothing

	Instructs DOS to do a reset of all it's disk buffers.

/****************************************************************************/


/****************************************************************************/
	int _dos_getdir( char far *Buffer, int Drive );

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\GET_DIR.ASM

	ARGUMENTS:	Buffer - Ptr to 64 byte memory	area
					Drive	- DOS drive number (0=default, 1=A, 2=B, ...)
	RETURN: 	int	- 0 if successfull else, !0 if error

	Gets the current directory path from DOS.
				  
/****************************************************************************/	


/****************************************************************************/
	long	_dos_seek( int Handle, long lOffset, int Mode )

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DOS_SEEK.ASM
	
	ARGUMENTS:	Handle	- Open DOS file handle
					lOffset - Offset to seek to in bytes
					Mode - Seek mode as described below
			  			0 = Beginning of file + offset
			  			1 = Current file position + offset
			  			2 = End of file + offset
	RETURNS:	long	- New offset in file is success
   		  	or -1L if error
/****************************************************************************/




/****************************************************************************/
	void EnableDiskAccess( unsigned char Drive )

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DRV_ACC.ASM

	ARGUMENTS: Drive - DOS drive letter (A=0, B=1, C=2, ...)
	RETURNS:   VOID

	Sets the drive access bit using undocumented
	function SET_ACCESS_FLAG (47h) of IOCtrl function
	44h.
/****************************************************************************/



/***************************************************************************/
	void Error( char **ErrorString )													
																								
	INCLUDE: BIOS_IO.H

	LOCATION: \PROMPTS\ERROR.C
	
	
	ARGUMENTS:	ErrorString 	- 		pointer to string identifing the error.
					CorrectionString - pointer to string detailing the error    
	RETURNS: 	void                                                        
																								
	EXTERNS:		ErrorMessageText	- Declared in EXTERN.C							
																								

	Non-fatal error function. An array of pointers to a string defining the 
	error and a pointer to a string suggesting a possible correction. Will  
	display an error message in a window on the screen and wait for a any   
	key to be pressed. A buffer to store the orginal contents under the     
	error window must be allocated at the start of the program since there  
	is no guarentee that there will be sufficient memory available when the 
	the error function is called.                                           

/***************************************************************************/





/***************************************************************************/
	void ExtractNextWord( char *szStr, char *szBuffer, int iBufLen )			
	
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\NEXTWORD.C
	
																					
	ARGUMENTS:	szStr - Ptr to string to parse word from						
					szBuffer - Buffer to copy the word into							
					iBufLen	- Total length of the buffer in bytes					
	RETURNS: 	void																			
																								

	Copies the first word from a string into a buffer with a specified max	
	length.																						
/***************************************************************************/






/****************************************************************************/
	int FcbParse( char far *OldName, struct FCB far *Fcb );

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\FCB_PARSE.ASM
	
	ARGUMENTS:	OldName - Ptr to string to parse
			Fcb	- Ptr to FCB struct to store
				  parsed name and drive.
	RETURNS:	int	- 00 - No wild card characters
				  01 - Wild card characters used
				  -1 - Invalid drive or filename

	Uses DOS FCB call 29h to parse a file name

	The following rules are observed based on the filename
	string argument. These are specifed by the bits in AL.
	when the int 21h is done.

	0. Ingores leading filename seperator
	1. Sets the drive number to 0 if not found in string
	2. Sets the filename to blanks if filename == ""
	3. Sets the file extension to blanks if not found
/****************************************************************************/



/****************************************************************************/
	int FcbRename( struct FCB far *Fcb );
	
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\FBC_REN.ASM

	ARGUMENTS:	Fcb	- Ptr to Special FCB structure of
				  the following form.

			BYTE(s) - CONTENTS
			0h	  - Drive number
			01h-08h - Old file name, padded with blanks
			09h-0bh - Old file ext., padded with blanks
			0ch-10h - All zeros
			11h-18h - New file name, padded with blanks
			19h-1bh - New file ext., padded with blanks
			1ch,24h - All zeros

	RETURNS:	int	- 0 if successfull else -1

	Uses DOS FCB call 17h to rename a file.
/****************************************************************************/






/***************************************************************************/
	int FindExtMatch( char *szFile, char **szExt )									
 
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\MATCHEXT.C

																								
	ARGUMENTS: szFile - Ptr to filename to chech for a matching extension	
	RETURNS: int	 - Index to matching extension in the list or -1		
																								

	Checks a file name to see if it's extension exists in a list of         
	different extensions. Returns the index to the first matching extension 

	NOTE: the filename may consist of the full path to the file.				
/***************************************************************************/






/***************************************************************************/
	int FileExists( char *szFile )														
				 
	INCLUDE: FILE_IO.H
																			
	LOCATION: \FILE\F_EXISTS.C
	
	ARGUMENTS:	szFile		- Ptr to full path name for file to be found 	
	RETURNS: 	int			- TRUE if file was found else FALSE 				

	Determines if the specified file exists.											

/***************************************************************************/





/***************************************************************************/
	int FindMaxFreePart( struct PartMap *Map )										
	
	INCLUDE: HDISK.H

	LOCATION: \HDISK\FINDPART.C

																							
	ARGUMENTS:	Map	- Ptr to partition map											

	RETURNS:		int	- Index to largest free area in the map or -1 if no	
						  	free area is found												
																								
	Scans a partition map and returns the index to the largest free area.	
	If no free partition area is found it returns -1. 
								
/***************************************************************************/





/***************************************************************************/
	int FindParam( char *szStr, char ch )												
  
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\FINDPARM.C

																								
	ARGUMENTS:	szStr 	- Ptr to string to be searched							
					ch 		- Parameter	character to search for 					
	RETURNS: 	int		- TRUE if search parameter	is found else false		
	
																							
	Searches a string for the specified character prefixed with a forward	
	slash character. The search for the parameter is not case sensitive. 	

/***************************************************************************/





/****************************************************************************/
	int FindString( char *Buffer, char *String, unsigned BufferSize );
 

	
	ARGUMENTS:
		Buffer  - The buffer to search
		String  - The zero terminated string to search for
		Count   - The length of the buffer in bytes
	RETURNS:
		 0 if succesful or -1 otherwise

	Quickly searches for a string in a buffer of any type of
	data. The search is case sensitive.

/****************************************************************************/





/***************************************************************************/
	void FmtStat( unsigned uTrack, unsigned uHead )									

	INCLUDE: BIOS_IO.H

	LOCATION: \PROMPTS\FMTSTAT.C

																								
	ARGUMENTS:	uTrack	- Specifies the current track being formatted or	
							  	if -1 signals a popup window should be removed	
					uHead		- Specified the cureent head being formatted or		
							  	if -1 signals a popup window should be displayed	
	RETURNS:		void																			
																								
	EXTERNS:		FmtStatText	- Declared in EXTERN.C as FMT_STAT_TEXT.			
																								

	This function is passed as an argument to the floppy disk format			
	which will call it indirectly to allow displaying the status of the 		
	formatting operation. The uTrack and uHead arguments specify the status	
	and are also used to indicate if the temporary window should be			
	displayed or removed.																	

/***************************************************************************/





/****************************************************************************/
	int	fnGetVideoInfo(void);

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\VIDTEST.ASM
	
	
	EXIT:
      	1 - CGA
      	9 - MCGA
      	8 - EGAHIRES_MONO
      	7 - EGA_COLOR
      	5 - VGA
     		-1 - FAILED
     		-2 - unknown value in BL after the first test.


	Determine the display adapter type using Int 10h function 1Bh.

/****************************************************************************/





/***************************************************************************/
	int FormatFloppy( unsigned uDrv, int iFmt, void (*vStatus)( int, int ))
																								
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\NEWFMT.C

	ARGUMENTS:	uDrv		Physical drive letter to format the disk on			
								which must be a floppy drive.								
					iFmt		Layout of the disk from the table above.				
					vStatus  Pointer to a function which will be called for		
								each time a track is formatted with the arguments	
								specifing the current track and head.					
																								
	RETURNS:		A valid error code or OK (0) if format was successfull.		
					The error code are the same as those returned by an iotcl	
					format.																		

	Formats a floppy diskette using int 13h function calls. Caller must		
	specify the drive number of 0 - 7fh and disk layout via a disk type of	
	0 - 6 as reflected in the table below.												
																								
	-------------------------------------------------------						
	0 | 160K  - 40 tracks - 08 sectors per track - 1 head							
	1 | 180K  - 40 tracks - 09 sectors per track - 1 head							
	2 | 320K  - 40 tracks - 08 sectors per track - 2 heads						
	3 | 360K  - 40 tracks - 09 sectors per track - 2 heads						
	4 | 720K  - 80 tracks - 09 sectors per track - 2 heads						
	5 | 1.2M  - 80 tracks - 15 sectors per track - 2 heads						
	6 | 1.44M - 80 tracks - 18 sectors per track - 2 heads						
																								
																								
/***************************************************************************/





/****************************************************************************/
	int GetBootSector( int Drive, char far *Buffer )
	
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\GET_BOOT.ASM
	

	ARGUMENTS:	Drive	- Physical drive number
			Buffer	- Ptr to sector size buffer

	RETURNS:	int	- OK (0) if successfull
				  else error code

	Reads the boot record on the first sector of the
	specified drive into the specified buffer
/****************************************************************************/






/****************************************************************************/
	int GetBreakSetting( void );

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\CTRLBRK.ASM

	ARGUMENTS:	NONE
  
	RETURNS:	int	- 0 if check is off else 1

	Returns the setting of DOS's internal Ctrl C check.

/****************************************************************************/





/****************************************************************************/
	unsigned GetConvMem( void );

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\GETMEM.ASM

	ARGUMENTS:	NONE

	RETURNS:	unsigned - Kbytes of convential memory

	Returns number of Kbytes of convential memory which is returned by int 12h.
/****************************************************************************/





/****************************************************************************/
	unsigned GetModelBytes( void );

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\GETMODEL.ASM

	ARGUMENTS:	NONE

	RETURNS:	unsigned - model byte in MSB
                       sub-model byte in LSB

	Returns model bytes as returned by int 15h, function c0h.
/****************************************************************************/





/****************************************************************************/
   unsigned IsConvertible( void );

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\GETMODEL.ASM

	ARGUMENTS:	NONE

	RETURNS:	unsigned - 1 if system is an IBM PC Convertible,
                       0 if not

   Checks model byte.
/****************************************************************************/





/****************************************************************************/
	void GetCountryInfo( struct COUNTRY_INFO *Cntry );

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\COUNTRY.ASM
	
	ARGUMENTS: Cntry - Ptr to 34 byte buffer
	
	RETURNS:   void

	Fills in a country information structure

/****************************************************************************/




/****************************************************************************;   _A386machine - return type of processor (386 vs. 8088/86/286).
	int GetCpuType( void );

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\GETCPU.ASM


	ENTRY:  (none)
	EXIT:   0 == 8086, 1 == 80286, 2 == 80386

	This routine relies on Intel-approved code that takes advantage
	of the documented behavior of the high nibble of the flag word
	in the REAL MODE of the various processors.  The MSB (bit 15)
	is always a one on the 8086 and 8088 and a zero on the 286 and
	386.  Bit 14 (NT flag) and bits 13/12 (IOPL bit field) are
	always zero on the 286, but can be set on the 386.

	CALLABLE FROM REAL MODE ONLY - near ROUTINE
/****************************************************************************/



/***************************************************************************/
	int GetDisketteType( int Drv )														
	
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_TYPE.C

																	
	ARGUMENTS:	Drv		- Drive letter to check										
	RETURNS:		int		- Type of diskette											
							  	1 == 360K														
							  	2 == 720K														
							  	3 == 1.2M														
							  	4 == 1.44M										
			
	Returns the type of diskette in the specied floppy drive.		
							  
/***************************************************************************/





/***************************************************************************/
	long GetDiskFree( int DrvLetter )													
	
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_FREE.ASM
																			
	
	ARGUMENTS:	DrvLetter - DOS drive letter										
	RETURNS:		long - Number of free bytes for the specified disk	
					    	or -1L if there is an error reading the disk	

	Returns the number of free bytes on the specified disk. Uses DOS get		
	disk free call for information about the disk and then calculates the	
	number of free bytes.								  
/***************************************************************************/




/***************************************************************************/
	int GetDiskHead( unsigned uSec, struct BPB *Bpb )								
																								
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_HEAD.C

	
	ARGUMENTS:	uSec		- Absolute disk sector										
					Bpb		- Ptr to BPB structure for disk being accessed.		
	RETURNS:		int		- The drive head which corresponds to uSec			

	Calculates the drive head which will access an absolute sector on			
	a disk.																						
/***************************************************************************/




/***************************************************************************/
	int GetDiskTrack( unsigned uSec, struct BPB *Bpb )								
									  
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_TRK.C


	ARGUMENTS:	uSec		- Absolute disk sector										
					Bpb		- Ptr to BPB structure for disk being accessed.		
	RETURNS:		int		- The drive track which corresponds to uSec			

	Caculates the track that an absolute sector is on.
																								
/***************************************************************************/


/***************************************************************************/
	int GetDisplayID(int MachineId)														
				 
	INCLUDE: BIOS_IO.H
																			
	LOCATION: \HARDWARE\VIDEO_ID.C

	Will return display type. Requires machineid.									
																								
	ENTRY: None 																				
	EXIT:	Returns Video adapter type 													
																								
/***************************************************************************/


/****************************************************************************/
	int GetDosDataVersion( void );


	INCLUDE: BIOS_IO.H


	LOCATION: \BIOS\DATAVERS.ASM
	
	
	Returns the DOS Data version from DOSDATA:04h

/****************************************************************************/


/****************************************************************************/
	int GetDrvParms( struct HdParms *Parms, int DrvNum )

	INCLUDE: HDISK.H

	LOCATION: \HDISK\HD_PARMS


	ARGUMENTS:	Parms	- Ptr to hdParms structure
					DrvNum	- Physical hard disk number
	RETURNS:	int	- 0 if successful
				- !0 if error (invalid drive)


	Gets the drive parameters for the specified hard
	and uses them to fill in the HdParms structure
	passed as an argument.

/****************************************************************************/





/****************************************************************************/
	int GetDriveType( unsigned char Drive, void far *Buffer )

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DRV_TYPE.ASM

	ARGUMENTS: Drive	- Physical drive number
		Buffer	- Ptr to work buffer at least 100 bytes

	RETURNS:	Drive type as defined by int 13h function 8.
		1	   360K   5.25"
		2	   1.2M   5.25"
		3	   720K   3.5"
		4	   1.44M  3.5"

	Determines the type of drive installed.
/****************************************************************************/




/***************************************************************************/
	unsigned GetExtMemSize ( void )

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\CMOS.ASM

	EXIT: extended memory amount

	Gets extended memory amount.
/***************************************************************************/



/***************************************************************************/
	int GetHelpFlags( void )																
							
	INCLUDE: HDISK.H

	LOCATION: \HDISK\DISPLAY.C

																	
	ARGUMENTS:	void																			

	RETURNS:		int - Bit flags specifing the current messages.			
																								

	Returns a bit flag of the current help line that is displayed on the 	
	bottom of the screen.

/***************************************************************************/





/***************************************************************************/
	long GetMaxHugeSize( void )															
	
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\MAXHUGE.C

																							
	ARGUMENTS:	NONE
																			
	RETURNS:		long - Number of bytes of huge memory avaiable			
																								

	Returns the size of the max block of huge memory that is available. The	
	amount available is determined doing a DOS call to allocate the largest	
	block avaliable and then freeing the allocated block and returning the	
	the size of the block that was allocated.											

/***************************************************************************/






/****************************************************************************/
	unsigned GetMemoryFree( void )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\MEMFREE.ASM


	EXIT: Number of free memory paragraphs available as a contiguos block
			from DOS


/****************************************************************************/




/***************************************************************************/
	void GetMessage( char **apszArray, unsigned uOffset )							
	
	INCLUDE: BIOS_IO.H

	LOCATION: \PROMPTS\MESSAGE.C

																							
	ARGUMENTS:	apszArray	- Array to hold the string pointers.				
					uOffset		- The offset of the first string in the			
								  	MESSAGE.TXT file as defined in the MESSAGE.H	
								  	file.														
	RETURNS:		void																			
																								
	EXTERNS:		chMessage	- First character in message area declared in 	
								  	MESSAGE.ASM by the message compiler.				

	Builds an array of pointers to the strings starting at a specified		
	offset. The caller should get the offset from the #defines in the			
	MESSAGE.H file which is created by the INDEX.EXE program. The the			
	array passed as argument should be long enought to hold the number		
	of lines store at the specified offset, this value is also generated		
	by the INDEX.EXE program and is included in the MESSAGE.H file. A NULL	
	pointer will be added to the array after the pointer to the last string	
	in the specified group.		 
															
/***************************************************************************/




/***************************************************************************/
	void GetMessStr( char *szBuf, unsigned TextOffset, int StrNum )			
		  
	INCLUDE: BIOS_IO.H

	LOCATION: \PROMPTS\MSTRING.C

																						
	ARGUMENTS:	szBuf 		- Ptr to buffer to copy the specified string to 
					TextOffset	- String group offset in the message file 		
					StrNum		- The group string number to get 					
	RETURNS: 	void																			
																								

	Copies a single string from the message area to the user's buffer.      

/***************************************************************************/






/****************************************************************************/
	int GetModelBytes	( void )

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\GETMODEL.ASM

	EXIT: int- hight byte contains model byte, low byte contain sub model

	Does INT 15h fuction C0h to determine the system model.
/****************************************************************************/




/****************************************************************************/
	int GetMouseID( void );

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\MOUSEID.ASM

	EXIT: mouse type -- UNKNOWN_MOUSE = 0, BUS_MOUSE = 1, SERIAL_MOUSE = 2,
	   	INPORT_MOUSE = 3, IBMPS2_MOUSE = 4, HP_MOUSE = 5, NO_MOUSE = 6

	Uses Int 33 to determine the mouse type.
/****************************************************************************/



/***************************************************************************/
	int GetNumHardDisks( void )

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\GETNUMHD.ASM
	
	ARGUMENTS:	NONE
	RETURN: 	int	- Number of hard disks installed

	Returns the total number of hard disks installed
	in the system as reported by the ROM BIOS at
	boot time.
/***************************************************************************/



/****************************************************************************/
	int	GetNumberOfDrives( void );

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\NUMDRVS.ASM
	
	ARGUMENTS:	NONE
	RETURN: 	int	- Number of drives installed

	Uses int 11h to return the total number of floppy
	drives in the system as reported by the ROM BIOS
	at boot time.
/****************************************************************************/




/***************************************************************************/
	int GetNumberStrings( char **Strings ) 											
 
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\NUMSTRS.C

	ARGUMENTS:	Strings	- Array of pointers to strings							
	
	RETURNS: 	int		- Total number of strings									


	Returns the number of strings in a array of strings. The last element   
	of the array must be a NULL pointer.                                    
/***************************************************************************/





/***************************************************************************/
	int GetOemDisplayID( void )															
																									
	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM
	
	ENTRY: None 																				
	
	EXIT:	Returns Video adapter type 													
			0 = MONO																				
			1 = CGA																				
			2 = EGA																				
			3 = VGA																				
			4 = HERCULES																		
	
	Will return display type. Requires machineid.									
/***************************************************************************/






/***************************************************************************/
	void GetPathStrings( char **apszPaths, char *chBuffer )						
	
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\PATHS.C

																							
	ARGUMENTS:	apszPaths	- Array of pointers to be filled in					
					chBuffer		- Buffer to copy the string into						
	RETURNS:		void																			

	Gets the PATH variable from the enviroment and then copies it to a		
	specified buffer and seperates the string into individual paths and		
	fills in an array of pointers to the beginning of each string.				
/***************************************************************************/





/***************************************************************************/
	int GetResponse( int ValidResponse[] )												

	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_RESPON.C

	ARGUMENTS:	ValidResponse - Array of valid keystrokes                  

	RETURNS: 	int - User's validated input                     


	Waits for a character to input by the user. The inputted character must 
	match a character in the argument array or an error beep will be        
	sounded. If the first element of the argument array is 0 the function   
	will accept any keystroke as a valid character and return. Case of the  
	characters in not consider as everything is converted to uppercase.     
																								
/***************************************************************************/







/***************************************************************************/
	unsigned long GetTotalHdSectors( struct HdParms *Parms )									
																								
	INCLUDE: HDISK.H

	LOCATION: \HDISK\GETSEC.C


	ARGUMENTS:	Parms 			- Ptr to hard disk parameters structure		
	RETURNS: 	unsigned long	- Total physical sectors on the hard disk		
																								

	Calculates the total sectors on a hard disk based on the values in a 	
	hard disk parameters structure.														


	NOTE: 																						
			Need to remember that heads and cylinders begin with 0 and			
			sectors begin with 1.															
																								
/***************************************************************************/




/***************************************************************************/
	int GetWinInfo( char **String, WINDOW *Window ) 								

	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_SIZE.C
																						
	ARGUMENTS:	Strings	-	Array of pointers to strings (NULL terminated)	
					Window	- Pointer to a WINDOW structure to be initialized	
	RETURNS:		int		- Number of strings which will fit in the window	


	Initializes a specified window structure based on the number and			
	length of an array of strings.														
																								
/***************************************************************************/





/****************************************************************************/
	int HardMouseDetect(void);

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\MOUSE.ASM
	
	
	EXIT: int -- mouse type or -1 if the hardware will not support mouse detection.     

	ALTERS  AX, BX, CX, DX, SI, DI


	This procedure what type of mouse is being used. It will determine
	that that type of mouse is present in the system and is working
	properly.  It calls: TestForInport, IRQAvailable, TestForBus, TestForSerial
	and TestForPS2

/****************************************************************************/




/***************************************************************************/
	void Help( void )																			
                                                                        
	INCLUDE: BIOS_IO.H

	LOCATION: \PROMPTS\HELP.C
	
	ARGUMENTS:	NONE																			

	RETURNS:		void																			
                                                                        
	EXTERNS:		HelpMemErrText	- Declared in EXTERN.C								
                                                                        
	Displays the help screen associated with the value on the help stack		
	pointed to by HelpStackPtr. Waits for the user to press a key and then	
	restores the original screen. Functions which want to use help should	
	push a help indentifier on the help stack with the macro PushHelp( x )	
	and then call GetChar() to get input from the user. If F1 is pressed 	
	GetChar will this function. If the value of *HelpStackPtr == -1 the 		
	function will return without displaying any text.								
 
/***************************************************************************/






/***************************************************************************/
	void HelpLine( int iFlags )															
	
	INCLUDE: HDISK.H

	LOCATION: \HDISK\DISPLAY.C

																							
	ARGUMENTS:	iFlags	- Bit flags specifing the messages to display.		
	RETURNS:		void																			
																								
	EXTERNS:		HelpLineText - Declared in EXTERN.C									
																								


	Displays a help line in reverse video on bottom line of the screen. The	
	text displayed on the line is specified by the flags passed as an arg	
	and defined by the string in the message file with the label				
	HELP_LINE_TEXT each bit in the argument represents a line from the text	
	group and are concatenated together and displayed as a single string.	
	

/***************************************************************************/





/****************************************************************************/
	int  Herc(void)

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\VIDTEST.ASM


	EXIT
      	0 - if it is not a Hercules.
      	1 - Herc 112
      	2 - Herc 222
      	3 - Herc 102



	Check for a Hercules. We are checking for a Herc102, 112, 222.
	This is the best possible test since the Hercules does not have any
	standard detection mechanism built into it.
	Taken from WORD code.

/****************************************************************************/



/***************************************************************************/
	char far *HugeAdd( char far *Ptr, long lBytes ) 								
	
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\HUGEADD.C

																							
	ARGUMENTS:	Ptr		- Far pointer which will be added to					
					lBytes	- Number of bytes to add to Ptr							
	RETURN:		char far * - New normalized pointer 								


	Adds a long integer to a far pointer without causing a segment wrap. 	
	The pointer that is returned will also be normalized so that the offset 
	is always < 16.																			
																								
/***************************************************************************/



/***************************************************************************/
	int IsWhite( char ch )																	
				 
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\ISWHITE.C

																				
	ARGUMENTS:ch - Character to be tested										
	RETURNS:	int - TRUE is arg is a DOS cmd line delimiter else FALSE	
																								

	Returns TRUE is the argument character is a valid DOS command line		
	delimiter.																					
	

/***************************************************************************/





/***************************************************************************/
	char far *NormalizePtr( char far *Ptr )											

	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\HUGENORM.C
	
																							
	ARGUMENTS: 	Ptr - Far pointer to be normalized						
	RETURNS:	char far *	- Normalized far pointer								


	Accepts a far pointer and returns a new pointer to the same location 	
	which has been normalize so that the offset is always < 16.					
/***************************************************************************/






/****************************************************************************/
	void InitNew13( char far *Buffer );

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\NEWINT13.ASM
	
	ARGUMENTS:	Buffer	- Far ptr to min 1024 byte buffer
			  	to be used for if a disk read
			  	needs to be split to avoid a
			  	DMA boundary error
	RETURNS:	void


	Sets int vector 13h to point to the NewInt13 function
	which does a check for DMA errors and corrects the
	error. Saves the original vector for later restoration.
/****************************************************************************/





/***************************************************************************/
	unsigned InsertChar( char *String, int Char )									
				
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\CHRINS.C

																				
	ARGUMENTS:	String		- Pointer to a string									
		   		Char			- character to be inserted at begining of string
																								
	RETURNS: 	unsigned 	- length of orignal string - 1						


	Inserts a character at the begining of a string and returns the new		
	length of the string.                                                   
																								
/***************************************************************************/






/****************************************************************************/
	void Int13WithRetry ( void )

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\INT13RTR.ASM

	Does an int 13h and resets the disk and does a retry
	if the first operation fails with an error other than
	a timeout error returns all register in the condition
	returned by the int 13h call.

/****************************************************************************/



/****************************************************************************/
	void Int24Fail( void );

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\FAIL24.ASM

	ARGUMENTS:	NONE
	RETURN: 	void	- Does not return to caller

	This function is can be called by an int 24h handler to
	simulate DOS failing the previous int 21h call. This
	function returns CPU control to the instruction following
	the last int 21h call. The registers are set exactly as
	they were when the int 21h was issued except the carry
	is set to signal an error and AL contains an error
	code to signal disk failure.
/****************************************************************************/




/****************************************************************************/
	int IsCEMMIn ( void )

	INCLUDE: FILE_IO.H

	LOCATION: \FILE\FGETCEMM.ASM

	EXIT: 1 if CEMM version 3.2 is installed, 0 otherwise

	Determines if CEMM version 3.2 is installed.
/****************************************************************************/



	

/***************************************************************************/
	int IsDBCSLeadByte(unsigned char c)

	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\DBCS.C


	ENTRY: char to compare with first byte of DBCS

	EXIT: TRUE if char is leadbyte, FALSE otherwise


	Test a character to see if it is a DBCS lead byte.

/***************************************************************************/



	

/***************************************************************************/
	int IsDirEmpty( char *szPath )														
				 
	INCLUDE: FILE_IO.H
																			
	LOCATION: \FILE\F_ISDIR.C

	ARGUMENTS:	szPath	- Ptr to full path name for directory to search 	
	RETURNS: 	int		- FALSE if file is found else TRUE						
																								
	Determines if any files exist in the specified directory path. 			
/***************************************************************************/



	
/***************************************************************************/
	int IsDiskReady( int Drive )
	
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_RDY.ASM

	ARGUMENTS:	Drive  - Physical drive number
	
	RETURNS:	int    - TRUE is disk is ready else FALSE

	Returns TRUE if a disk is in the specified drive
	else returns FALSE. Uses BIOS int 13h to try to
	verify the first sector on the disk and then checks
	the return code if an error is detected and returns
	TRUE if error != 0x80 else returns FALSE if time out
	error is returned. If changeline is returned the
	verify is retried to avoid conflicts.
/***************************************************************************/



/***************************************************************************/
	int IsDos4Compat( void )																
																								
	INCLUDE: HDISK.H

	LOCATION: \HDISK\IS_DOS4.C

	ARGUMENTS:	NONE																			

	RETURN:		int	- TRUE is BPB is DOS 4.x compatible else FALSE			
																								
                                                                        
	Determines if a BPB in a boot record is compatible with DOS 4.x. The 	
	boot sector must have already been read in the global sector buffer. 	
	First checks 512 bytes per sector, then maximum root directory entries	
	and then makes sure there is not a conflict in the total number of		
	sectors. 																					

/***************************************************************************/




/***************************************************************************/
	int IsDosPart( unsigned char PartitionType ) 									
																								
	INCLUDE: HDISK.H

	LOCATION: \HDISK\ISDOSPRT.C

	ARGUMENTS:	PartitionType	- Partition type as found in partition table 
	
	RETURNS: 	int				- TRUE if DOS primary partition else false	
																								

	Checks a partition entry type to see if it is a valid DOS primary 		
	partition.																					
																								
	VALID PARTITION TYPES:																	
																								
	DOS12  == 1 == FAT file system - 12 bit FAT										
	DOS16  == 4 == FAT file system - 16 bit FAT										
	DOSNEW == 6 == FAT file sytem	 - huge partition > 32 meg						
																								

/***************************************************************************/






/***************************************************************************/
	int IsFmtedBoot( char *SectorBuf )													
																								
	INCLUDE: HDISK.H

	LOCATION: \HDISK\FMTBOOT.ASM

	ARGUMENTS:	SectorBuf	- Ptr to buf holding the boot sector to check	
	
	RETURNS: 	int			- TRUE if a formatted boot record else FALSE 	
	
																							
	Determines if a boot record if from a partition that was formatted		
	under any version of DOS. Checks for proper opcode in the first 3 bytes 
	(e9,XX,XX or eb,XX,90).																	

/***************************************************************************/






/***************************************************************************/
	int IsFormatted( int iDrv )															

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_ISFM.C
																								
	ARGUMENTS:	iDrv	- Physical drive number											
					Bpb	- Ptr to BPB struct to be filled in							
					pchBuffer - Ptr to work buffer at least SECTOR_SIZE 			
	RETURNS:		int	- TRUE if disk is formatted else false						
																								
	GLOBALS:		DskBpb - Array of valid BPB structures in DSK_BPB.C				

	Determines if the disk in the specified drive has been formatted by		
	reading in the boot record and checking the BPB with all normal floppy	
	disk BPB layouts. If there is an error reading the boot record or if		
	the BPB is not valid returns FALSE else TRUE. If the disk is formmatted	
	the Bpb struct specified by the argument Bpb will be filled in. Before	
	this function is called to IsDiskReady should be done to be sure that	
	there is a disk in the drive.		
/***************************************************************************/





/***************************************************************************/
	int IsHimemDrvPresent ( void )

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\GETHIMEM.ASM

	EXIT: 1 if HIMEM.SYS is already installed, 0 otherwise

	Determines if HIMEM.SYS is installed.
/****************************************************************************/




/****************************************************************************/
	int IsNotIBM ( void )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\IBM_ID.ASM
	
	EXIT: 0 if ROM contains IBM basic, !0 otherwise
	
	Check for a ROM containing IBM basic.

/****************************************************************************/




/****************************************************************************/
	int IsTsengLabsVGA( void )

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\VIDEOTEST.ASM

	EXIT:  TRUE  = Tseng Labls VGA is present, FALSE otherwise

	Function will detect Tseng Labs VGA card. This is necessary because if
	the Chips & Technologies detection code is executed on the Tseng Labs VGA
	card it's game over man !! Hang city !	Detection works on the principal that 
	the Attribute Controller Reg 16h is	known to exist only on Tseng Labs VGA

/****************************************************************************/





/***************************************************************************/
   int IsValidPath( char *szPath, unsigned DrvNumber, int SavePath )
	
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_DIR.C

   ARGUMENTS:	szPath	- A directory path string
   				DrvNumber - DOS drive number: 1 = A, 2 = B, 3 = C, ...
   				SavePath - TRUE if part of path that is created is to be
   								kept, FALSE if it	is to be deleted.
   RETURNS:		int		- TRUE if a valid path else FALSE

   Validates a directory path string by cleaning up the string and then
   creating the directory path. If the path can be successfully created it
   is assumed to be a valid path. If there is an error creating the path
   it is assumed the path name is invalid and any part of the path that
   did not exist originally is deleted. The path may contain a trailing
   '\' character but it is not necessary as one will be added by this
   function if it was omitted. If SavePath is FALSE, the part of the path
   that has been created is deleted. The current path is is saved on entry
   and restored before the function returns to the caller.
/***************************************************************************/



/***************************************************************************/
	IsValidPathChar( char Char )															

	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\PATHCHR.C

																								
	ARGUMENTS:	Char	- Character to be tested										
	RETURNS: 	int	- TRUE if a valid character else FALSE 					


	Validates a character as a valid path and file name character. 			

/***************************************************************************/




/****************************************************************************/
	int  KbdGetKey( void );

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\KEYBOARD.ASM

	EXIT: high byte contains scan code, low byte contains ASC character of key
			pressed.
	
	Waits for a character from the keyboard returns it's value and the keyboard
	scan code.

/****************************************************************************/




/****************************************************************************/
	int  KbdGetStatus( void );
	
	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\KEYBOARD.ASM
	
	EXIT: Status of various keyboard toggles and shift keys.

	Does INT 16h function 2 to get the keyboard status.

/****************************************************************************/




/****************************************************************************/
	int  KbdIsWaiting( void );
	
	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\KEYBOARD.ASM

	Checks for a character waiting in the keyboard buffer
	Returns 0 in AX if no character is waiting.
	The character is not removed from the buffer and will
	be returned by the next call to KbdGetKey

/****************************************************************************/




/****************************************************************************/
	void interrupt cdecl far NewInt24 (unsigned es, unsigned ds,
				unsigned di, unsigned si, unsigned bp, unsigned sp,
				unsigned bx, unsigned dx, unsigned cx, unsigned ax )

	INCLUDE: DISK_IO.H			
				
	LOCATION: \DISK\DSK_I24.C

	ARGUMENTS:	es, ds, di, si, bp, sp, bx, dx, cx, ax registers

	Interrupt 24h handler. Checks to see if the error is drive door open 	
	or write protect error on a floppy disk and will prompt the user to		
	remedy the problem and then return to DOS for a retry of the operation
	else will issue a fatal disk error and abort the program.					
/****************************************************************************/




/***************************************************************************/
	int IndexMaxInt( int aiMatch[], int iLen )

	INCLUDE: STRLIB.H

   LOCATION: \STRLIB\MAXINT.C

                                                     
	Scans an array of integers and returns the index to the max int in the	
	array. If 2 or more integers equal MAX the index to the first one in		
	the array is used. If all elements in the array are 0 a -1 is returned.	
																								
	ARGUMENTS:	aiMatch - Array of integers to search.							
					iLen - Number of elements in the array.						
	RETURNS:		int - First maximium value in the array or -1 if all	
					   	elements == 0.												
                                                                        
/***************************************************************************/



/***************************************************************************/
	unsigned MaxStrLen( char *Strings[] )												

	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\STRMAX.C

																								
	ARGUMENTS:	Strings - array of pointers to strings								

	RETURNS: 	unsigned - length of the longest string in the array			
																								

	Returns the length of the longest string in an array of strings.			

/***************************************************************************/




/***************************************************************************/
	void NewScreen( unsigned Text, unsigned Help )									
	
	INCLUDE: BIOS_IO.H

	LOCATION: \PROMPTS\MESSAGE.C

																							
	ARGUMENTS:	Text		- Offset of message in the text file					
					HelpFlags- Bit flags for desired help line messages			
	RETURNS:		int		- Number of strings displayed.							
																								

	Clears the screen and displays the messages from the text file as			
	specified by the Text argument and then displays the help line as 		
	specified by the HelpFlags argument.												

/***************************************************************************/




/***************************************************************************/
	void PadStr( char *szStr, char chChar, int Len )								

	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\STRPAD.C
																								
	ARGUMENTS:	szStr 	- Ptr to string												
					chChar	- Character to pad the string with						
					Len		- Total length of the padded string in bytes			

	RETURNS: 	void																			
																								
	Pads the end of a string with a specified character.							

/***************************************************************************/





/***************************************************************************/
	char *ParseFileName( char *szPath ) 												
	
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\PARSEFN.C

	ARGUMENTS:	szPath - Ptr to a file path in the form d:\xxxx\xxx.xxx	

	RETURNS: 	char * - Ptr to file name or character after last			
						   	backslash or ':' in the string if the path did	
						   	not contain a file name									

	Returns a pointer to the first character in the filename which may or	
	may not be appended to a path.														
	
							  
/***************************************************************************/




/****************************************************************************/
	int PhyDiskRead( char far *pchBuf, int iSecCyl,
			  int iHead, char chDrive,
			  int cSecs);

	INCLUDE: DISK_IO.H		  
			  
	LOCATION: \DISK\DSKREAD.ASM

	ARGUMENTS:
		pchBuf -- buffer to read the disk into
		iSecCyl -- low byte contains starting sector, high byte contains
					  starting cylinder
		iHead -- head to use
		chDrive -- drive to use	 (00h - 7Fh = floppy, 80h - FFh = fixed disk)
		cSecs -- number of sectors to read
	EXIT:
		0 if succesful, 1 otherwise

	Uses the ROM BIOS int 13h read sectors function
	to read the specified number of sectors.
/****************************************************************************/


	

/****************************************************************************/
	int PhyDiskWrite( char far *pchBuf, int iSecCyl,
			  int iHead, char chDrive,
			  int cSecs);

	INCLUDE: DISK_IO.H
			  
	LOCATION: \DISK\DSKWRITE.ASM

	ARGUMENTS:
		pchBuf -- buffer to write to disk 
		iSecCyl -- low byte contains starting sector, high byte contains
					  starting cylinder
		iHead -- head to use
		chDrive -- drive to use	 (00h - 7Fh = floppy, 80h - FFh = fixed disk)
		cSecs -- number of sectors to write
	EXIT:
		0 if succesful, 1 otherwise

	Uses the ROM BIOS int 13h write sectors function
	to write the specified number of sectors.
/****************************************************************************/




/***************************************************************************/
	void ProcessDiskError( int ErrorType ) 											
																								
	INCLUDE: HDISK.H

	LOCATION: \HDISK\DSKERROR.C

	ARGUMENTS:	NONE												
							
	RETURN:		void																			
																								
	EXTERNS:		DiskErrorText	- Declared in EXTERN.C								
																								

	Displays an error message based on a disk error passed as an argument.	

/***************************************************************************/




/***************************************************************************/
	int PromptForDefault( void )															
	
	INLCUDE: WINDOW.H

	LOCATION: \PROMPTS\DFLTPRMT.C

	ARGUMENTS:	NONE																			
	
	RETURNS: 	int		TRUE is user selects YES else FALSE 					
																								
	EXTERNS:		AcceptText 		- Defined in extern.c								
					SetDefaultText	- Defined in extern.c								
																								

	Displays a prompt asking the user if they want to use the disk format	
	they have previously selected as the default for the rest of the			
	program. 																					

				
/***************************************************************************/





/**************************************************************************/
	int PromptWindow( char **String, int *ValidResponse, int wColor, 			
                 	char *Buffer )														
 
	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_PROMPT.C                 
                 
                    
	ARGUMENTS:	String 			- Array of ptrs to strings to be displayed	
									  	in the window,the last element of the		
									  	array must be a NULL pointer           		
					ValidResponse 	- Array of ints which specify valid input		
									  	from the user. If the first element of the	
									  	array is 0 the function will not validate	
									  	the input and will return after any key is	
									  	pressed. If ValidResponse == NULL no input	
									  	is waited for and the window is left			
									  	displayed												
					wColor 			- color of the window to be displayed        
					Buffer 			- a ptr to buffer large enough to hold the	
									  	contents of the orginal display under the	
									  	window, if Buffer == NULL the function		
									  	will allocate a buffer to hold the data.   
	RETURNS: 	int				- Character input by the user                
																								

	Displays an untitled window on the screen and prompts waits for input   
	from the user that matches an entry in a validation array specified by  
	the caller. The length and width of the window are calculated based on  
	number and length of the strings passed to the function.                


/***************************************************************************/





/***************************************************************************/
	void  PutTitledWindow( struct WindowStruct *Window, char *Title )			
                                                                        
	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_TWIN.C
	
	
	ARGUMENTS:	Window	- Ptr to initialized window definition structure.  
					Title 	- Ptr to a title string                            
	RETURNS:		void																			
																								

	Displays a titled window in the screen as define in the window          
	definition structure and displays the window's title.                   

/***************************************************************************/





/***************************************************************************/
	void PutWindow ( struct  WindowStruct *Win )										
                                                                        
	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_PUTWIN.C
	
	ARGUMENTS:	Win	- Ptr to initialized window definition structure.     

	RETURNS:		void																			


	Displays an un-titled window in the screen as define in the window      
	definition structure.                                                   
																								
/***************************************************************************/






/***************************************************************************/
	int RdWrSectors( int iDrv, unsigned uSec, unsigned uNumSec,					
					  	char *puchBuf, struct BPB *Bpb, int RdWr )					

  	INCLUDE: DISK_IO.H
						
	LOCATION: \DISK\DSK_SECT.C
																									
	ARGUMENTS:	unsigned iDrv		Physical drive to write to						
					unsigned uSec		Absolute starting sector to begin at		
					unsigned uNumSec	Number of sectors to write						
					int		iFmt		Disk type listed in first header				
					char 		*puchBuf	Ptr to buffer holding data to write			
					struct BPB *Bpb	BPB struct for disk in drive					
					int		RdWr		Specifies read (0) or write (1)				
																									
	RETURNS:		int					Error code or OK if successfull				

	Reads or writes the specified number of sectors to a floppy disk			
	using interrupt 13h calls. 															

/***************************************************************************/





/***************************************************************************/
	int ReadPartBootRec( struct Part *PartEntry, void *Buffer,
			     int DrvNum );

	INCLUDE: HDISK.H

	LOCATION: \HDISK\PARTBOOT.ASM
			  
				  
	ARGUMENTS:	PartEntry - Partition table entry structure
			Buffer	- Ptr to read buffer of 512 bytes
			DrvNum	- Physical hard disk number
	RETURNS:	int	- 0 if successful
				- !0 if error


	Reads in the first sector (boot	record) of a hard
	disk partition.


/***************************************************************************/





/***************************************************************************/
	int ReadWriteBoot( int iDosDrv, void *Buffer, int ReadWrite )			  
																								  
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\RW_BOOT.C
	
	ARGUMENTS:	iDosDrv	- DOS drive number to get boot record from		  
					Buffer	- Ptr to buffer to hold the boot record			  
	RETURNS:		int		- OK if successful else a DOS error code for		  
	
	Uses int 25h&26h to read or write the boot record for the specified	  
	drive.																					  
/***************************************************************************/




/***************************************************************************/
	ReadWriteFat( int iDosDrv, struct BPB *Bpb, void *Buf, int Sector,		
				  	int ReadWrite )															
																		
	INCLUDE: DISK_IO.H
					
	LOCATION: \DISK\RW_FAT.C					
	
	ARGUMENTS:	iDosDrv	- The DOS drive number to get the sector from		
					Bpb		- Ptr to bpb structure for the specified drive		
					Buf		- Ptr to sector buffer										
					Sector	- The FAT sector number based 0							
					ReadWrite- Flags reading or writing sector - READ or WRITE 	
	RETURNS:		int		- OK if successfull else int 25h error code			

	Reads or writes a specified FAT sector on the specified drive.	Will		
	update the same sector in all copies of the FAT.	
	
/***************************************************************************/





/***************************************************************************/
	ReadWriteRoot( int iDosDrv, struct BPB *Bpb, void *Buf, int Sector		
				  				int ReadWrite )												
																								
	INCLUDE: DISK_IO.H
								
	LOCATION: \DISK\RW_ROOT.ASM
								
	ARGUMENTS:	iDosDrv	- The DOS drive number to get the sector from		
					Bpb		- Ptr to bpb structure for the specified drive		
					Buf		- Ptr to sector buffer										
					Sector	- The root directory sector number based 0			
					ReadWrite- Flags reading or writing sector - READ or WRITE 	
	RETURNS:		int		- OK if successfull else int 25h error code			

	Reads or writes a specified root directory sector on the specified drive.	
	
/***************************************************************************/




/****************************************************************************/
	void RebootSystem( void );

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\REBOOT.ASM

	Forces a system power on reboot.

/****************************************************************************/




/***************************************************************************/
	unsigned RemoveSpaces( char *szString )											

	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\STRRMSP.C
																								
	ARGUMENTS:	String - pointer to string											

	RETURNS: 	unsigned - length of the new string									
																								

	Removes all spaces in a string.														

/***************************************************************************/





/***************************************************************************/
	unsigned RemoveTrailing( char *String, char Char ) 							
																								
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\RMTRAIL.C

	ARGUMENTS:	String - pointer to a string										
					Char - the ascii char to remove from the end of string	
																								
	RETURNS: 	unsigned - length of the new string									
																								

	Removes all trailing characters of the type specified from a string		
	and returns the length of the new string.                               
/***************************************************************************/




/***************************************************************************/
	int RenameFCB( char *szFrom, char *szTo )											

	INCLUDE: FILE_IO.H

	LOCATION: \FILE\F_REN.C
																							
	ARGUMENTS:	szFrom	- Original name of file in the form X:\NAME.EXT 	
					szTo		- New name for	file in the form X:\FILENAME.EXT		
	RETURNS: 	int		- OK if successfull else error							


	Renames within the same directory of the specified full drive and path	
	anem. This is necessary when renameing the system files because DOS		
	versions < 3 move the physical directory entry when renaming which will 
	move the system file entries out of the first 2 positions.					
/***************************************************************************/





/***************************************************************************/
	void ReplaceChar( char *szString, char chOldChar, char chNewChar )		
	
	LOCATION: STRLIB\CHRREPL.C
																							
	ARGUMENTS:	szString 	- Ptr to string											
					chOldChar	- Char to be replaced									
					chNewChar	- Replacement character 								
	RETURNS: 	void																			
																								

	Replaces all occurances of a specified character in a string with 		
	another character.																		
	
/***************************************************************************/






/***************************************************************************/
	int ReplaceFile( char *szSource, char *szDestin )								
				 
	INCLUDE: FILE_IO.H

	LOCATION: \FILE\F_REPLACE.C

	ARGUMENTS:	szSource - Ptr to full path and name for source file			
					szDestin - Ptr to full path and name for destination file	
	RETURNS: 	int		- OK if successful else ERROR 							

		
	Replaces the destination file with the source file by first seeing if	
	the source exists and then deleting the destination and renameing the	
	destination and renaming the source to take it's place. The check for   
	the destination is done to be sure the replace has not already taken 	
	place.																																														

/***************************************************************************/




/****************************************************************************/
	void ResetDrv( int Drive );

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\BIOSRSET.ASM
	
	ARGUMENTS:	Drive	- Physical disk drive number.

	RETURNS:	void

	Does a ROM BIOS drive reset on the specified drive.
/****************************************************************************/





/***************************************************************************/
	void RestoreCursor( unsigned long Cursor )										
	
	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_RSTCUR.C

	ARGUMENTS:	Cursor	- Cursor size & position returned by SaveCursor()	
	
	RETURNS:		void																			


	Displays the cursor at the screen position and size specified in the		
	argument as an unsigned long with the  position in the high byte and		
	size in the low byte.																	
																								
/***************************************************************************/




/***************************************************************************/
	unsigned long SaveCursor ( void )													
	
	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_SAVCUR.C

																							
	ARGUMENTS:	NONE																			
	
	RETURNS:		unsigned long	- Cursor screen position and size				

	Returns the size and position of the cursor as an unsigned long value 	
	with the position in the high byte and size in the low byte.				
																								
/***************************************************************************/





/***************************************************************************/
	int ScrubFatRoot( int iDrv, struct BPB *Bpb )									
																								
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_SCRB.C
	
	ARGUMENTS:	iDrv	-	Physical floppy drive number								
					Bpb	-	Ptr to Bpb struct for the disk being scrubbed		
	RETURNS:		int	-	OK if successfull else !OK									

	Empties the FAT and root directory to make the disk look freshly			
	formatted by zeroing all good clusters in all copies of the FAT and		
	then zeroing out all entries in all root directory sectors.					
/***************************************************************************/





/***************************************************************************/
	void SetDefaultColors( int IsColor )												
																								
	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_COLOR.C

	ARGUMENTS:	NONE																			

	RETURNS:		void																			

	Sets the default colors based on whether IsColor is TRUE or FALSE.	
/***************************************************************************/






/***************************************************************************/
	int SetDiskType( unsigned uDrv, unsigned char DiskType );

	INCLUDE: DISK_IO.H

	LOCATION: \DISK\SETDSKTY.ASM

	ARGUMENTS:	uDrv	- Physical floppy drive number
			DiskType - Extended BIOS disk type
			0	- Not used
			1	- 320/360K in 360K drive
			2	- 320/360K in 1.2M drive
			3	- 1.2M in 1.2M drive
			4	- 720K in 720K drive
	RETURN:		int	- 0 if ok else INVALID_DRIVE_TYPE

	Extended ROM BIOS call which sets a disk type for
	formatting using function 17h of int 13h. The function
	must do 1 retry in case disk changed status is returned.

/***************************************************************************/





/***************************************************************************/
	void SetFileTimeDate( struct DIR *Dir )											
															
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_TIME.C
									
	ARGUMENTS:	Dir	- Ptr to directory entry structure							
	RETURNS: 	void										 
	
	Sets date and time of file to current date and time.							
																									
/***************************************************************************/



/***************************************************************************/
	void far *SetMediaType( int Drive, int Tracks, int Sectors )

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\SETMEDIA.ASM
	
	ARGUMENTS: Drive	- Physical drive number
		   Tracks	- Total tracks on the floppy disk
		   Sectors	- Number of sectors per track

	RETURNS:		  Ptr to DASD for this type of drive
				  is successfull else NULL ptr

	Extended ROM BIOS call to set the disk layout for a
	format operation. The function must do 1 retry in case
	a disk change status is returned.
/***************************************************************************/




/***************************************************************************/
	int SetNewBpb( struct BPB far *Bpb, char far *WorkBuffer, int Drive );

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\SETBPB.ASM
	
	ARGUMENTS:	Bpb	- The new BPB structure
			Buffer	- A work buffer for use by the function
			Drive	- The disk drive to set the new bpb for
	RETURNS:	int	- 0 if successfull else -1

	Uses the generic IOCTL call to set the default bpb for a	specified 
	disk drive

/***************************************************************************/




/***************************************************************************/
	unsigned ShiftStringLeft( char *String )											
	
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\STRSHL.C
																							
	ARGUMENTS:	String	- pointer to string to be shifted						
	
	RETURNS: 	unsigned - length of original string - 1							
																								

	ShiftStringLeft() moves a string left one character, including EOL.		
	Returns the length of the new string.                                   
	
/***************************************************************************/




/***************************************************************************/
	char *SkipLeadingWhite( char *szPtr )												
			 
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\SKIPSPC.C

	ARGUMENTS: Str - Ptr to a string												

	RETURNS: char*	- Ptr to first non-white space character or EOL 	

	Returns a ptr to first character after any white spaces or '=' chars.	
	Checks for EOL and if encountered stops processing and returns a ptr 	
	to the EOL character.																	
/***************************************************************************/





/***************************************************************************/
	char *SkipNonWhite( char *szPtr )													
																								
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\NONWHITE.C
	
	ARGUMENTS:	Str		- Ptr to a string												

	RETURNS: 	char *	- Ptr to first white space character or EOL	
																								

	Returns a ptr to first white space or '=' character encountered.			

/***************************************************************************/





/***************************************************************************/
	char *SkipWord( char *szPtr ) 														
						
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\SKIPWORD.C
																		
	ARGUMENTS:	szPtr - Ptr to string												

	RETURNS: 	char * - Ptr to first character of next word or EOL 		
			  																							

	Returns a ptr to the first character of the word following the first 	
	encountered in the specified string.												
/***************************************************************************/






/***************************************************************************/
	int StrSearch( char *szSearchStr, char **szStrings ) 							
																								
	INCLUDE: STRLIB.H

	LOCATION: \STRLIB\STRSRCH.C

	ARGUMENTS:	szSearchStr	- String to match											
					szStrings	- Array of pointers to strings with last the		
								  	end of the array marked with a NULL pointer	
	RETURNS:		int			- Index to the first matching string or -1 if	
								  	there was no matching string found.				
                                                                        
	
	Looks for a matching string in an array of pointers to strings.			
/***************************************************************************/







/****************************************************************************/
	void SwapDiskTable ( void )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\TRS_TOG.ASM

	Swaps the 26 bytes in Assign's disk table with those
	in our disk table.

/****************************************************************************/




/****************************************************************************/
	void ToggleAppend( void )
	
	INCLUDE: BIOS_IO.H

	
	LOCATION: \BIOS\TRS_TOG.ASM
	
	Checks to see if APPEND has been installed and if it has
	toggles it active or inactive depending on it's current
	state.
/****************************************************************************/


/****************************************************************************/
	void ToggleAssign( void )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\TRS_TOG.ASM
	
	Checks to see if ASSIGN has been installed and if it 
	has toggles it active or inactive depending on it's
	current state.
/****************************************************************************/



/***************************************************************************/
	unsigned long ToLongTd( struct find_t *File )									
																								
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_TCNV.C
	
	ARGUMENTS:	File		- Ptr to a filled in find_t structure					
	RETURNS:		long		- Date in most sig. byte and Time in least sig.		

	Converts the time and date returned by the a call to _dos_findfirst		
	int a long value with the date in the most signifiant byte and the time	
	in the least sig. bytes. This allows for simple and accurate file			
	creation comparisions.																	
/***************************************************************************/



/****************************************************************************/
	void UpdateBreakSetting( int OnOffFlag );

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\SETBRK.ASM
	
	ARGUMENTS:	OnOffFlag - TRUE if turning on else FALSE
	
	RETURNS:	void

	Sets the DOS internal break check setting to on or off.
/****************************************************************************/




/***************************************************************************/
	int	ValidateDir( char *szPath )													
																									
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\V_DIR.C
	
	ARGUMENTS: szPath	- The path string to use to find the next directory
	
	RETURNS: int - OK if successful else ERROR								

	Parses out the first directory from a path string and validates that 	
	directory name and if valid recursively calls itself until each name	in	
	in the path has been validated or an error is detected.						

/***************************************************************************/




/***************************************************************************/
	int ValidatePath( char *szPath )														
	
	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\V_DIR.C
																							
	ARGUMENTS:	szPath	- A directory path string									
	
	RETURNS:		int		- TRUE if a valid path else FALSE						

	Validates a directory path string by cleaning up the string and then		
	checking that each directory in the path has a valid directory name.		
	
/***************************************************************************/




/****************************************************************************/
	int	VideoAdapter(void);

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\VIDEO_ID.C
	
	Returns in AX:
		1 - CGA
		9 - MCGA
		8 - EGAHIRES_MONO
		7 - EGA_COLOR
		5 - VGA
		-1 - FAILED
		-2 - unknown value in BL after the first test.

	Determine the display adapter type using int 10h function 1Bh
/****************************************************************************/




/****************************************************************************/
	void VideoBlockCurs ( void )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM
	
	Makes the cursor a block cursor.
/****************************************************************************/






/***************************************************************************/
	void VideoCleanup( void )																

	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_CLEAN.C

	ARGUMENTS:	NONE																			
	
	RETURNS:		void																			
																							
	Function to clear the screen and restore the cursor to normal size and	
	top left corner position.																
/***************************************************************************/






/****************************************************************************/
	void VideoCls( int Attrib )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM

	ARGUMENTS
		attrib -- attribute of blanked characters

	Blanks the entire screen using a specified attribute.
/****************************************************************************/



/****************************************************************************/
	void VideoCursOff ( void )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM

	Turns the text mode cursor off.
/****************************************************************************/



/****************************************************************************/
	void  VideoDupAttr( int Row, int Col, int Attr, int DupFactor )

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM
	
	ENTRY
		row -- row to start changing attribute
		col -- column to start changing attributes
		attr -- attribute to set characters to
		dupfactor -- number of characters in the line

	Changes the attributes of a horizontal line of characters on the display 
	and rewrites it with the new attribute set.

/****************************************************************************/




/****************************************************************************/
	VideoDupCharAttr (int Row, int Col, int DupChar, int Attrib, int DupFactor)

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM
	
	ENTRY
		row -- row to start writing character
		col -- column to start writing character
		dupchar -- character to be written
		attrib -- attribute of character to be written
		dupfactor -- number of characters in the line

	Writes a horizontal line of characters on the display.
/****************************************************************************/




/****************************************************************************/
	void VideoGetBlock( int Row, int Col, char *awSource, int Count )

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM


	ARGUMENTS
		row -- starting row of block to copy from
		col -- starting column of block to copy from
		awSource -- ptr to buffer to copy block to
		count -- number of characters to copy

	Copies a horizontal block of characters and their attributes to a buffer.
 	Each character is represented as a word with the high byte containing the
	attribute and the low byte the ASCII code for the character.
/****************************************************************************/





/****************************************************************************/
	int VideoGetCursSize (void)

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM
	
	RETURNS
		cursor size --	bits 0-4 of high byte contain	starting line for cursor 
							and bits 0-4 of low byte contain	ending line for cursor. 
	
	Gets the current cursor size.
/****************************************************************************/




/****************************************************************************/
	void VideoGetMode( void );
	
	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM
	
	Returns current video mode and does setup of the
	static variables containing screen setup
/****************************************************************************/




/****************************************************************************/
	int VideoGetRowCol ( void)

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM
	
	RETURNS
		location of cursor position: row is in top byte and column in bottom

	Gets current cursor position. 
/****************************************************************************/



/****************************************************************************/
	void	VideoInitial( void );
	
	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM
	
	Initializes the video data, must be called before any
	other video function

/****************************************************************************/





/****************************************************************************/
	int VideoIsColor ( void )

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM
	
	Returns true if a color card is active.
/****************************************************************************/


/****************************************************************************/
	void VideoNormalCurs ( void )

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM
	
	Makes the cursor an underlin cursor.
/****************************************************************************/



/****************************************************************************/
	void VideoPutBlock( int Row, int Col, char *awSource, int Count )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM
	
	ARGUMENTS
		row -- starting row of block to copy to
		col -- starting column of block to copy to
		awSource -- ptr to buffer to copy block from
		count -- number of characters to copy	

	Copies the characters and their attributes from a buffer to a horizonal 
	block on the screen.  The characters are stored in the block as words with
	the high byte containing the attribute and the low byte the ASCII code for
	the character.
/****************************************************************************/




/****************************************************************************/
	void VideoPutChar( char Character);

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM

	Displays a character at the current cursor position on the screen and
	advances the cursor to the next character position.
/****************************************************************************/




/****************************************************************************/
	VideoPutCharAttr( Row, Col, Character, Color )
	
	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM
	
	Displays a character at the specified row and column on the screen  with
	the specifed screen attribute. Cursor will end up at the specified row
	and column position.
/****************************************************************************/


/****************************************************************************/
	void VideoPutCharRowCol( int Row, int Col, int Character )

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM

	Displays a character at the specified row and column on the screen.
	Cursor will end up at the specified row and column position.
/****************************************************************************/



/****************************************************************************/
	void	VideoPuts( char far *String )

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM
	
	ENTRY
		string -- pointer to a string

	Writes string to current position on the screen.
/****************************************************************************/



/****************************************************************************/
	void  VideoPutsAttrRowCol( int Row, int Col, char far *String, int Attr )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM

	ENTRY
		row -- row at which to output string
		col -- column at which to output string
		string -- pointer to string to be written
		attr -- attribute of chars of string

	Writes a string to the current position on the display.  All characters
	are written with the specified attribute.
/****************************************************************************/




/****************************************************************************/
	void  VideoPutsRowCol( int Row, int Col, char far *String )

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM

	ENTRY
		row -- row at which to output string
		col -- column at which to output string
		string -- pointer to string to be written

	Writes string to a specified location
/****************************************************************************/



/****************************************************************************/
	void VideoScrollDn( int StartRow, int StartCol, int EndRow, int EndCol,
	                    int Lines, int Attrib )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM

	ENTRY
		startrow -- starting row of window to be scrolled
		startcol -- starting column of window to be scrolled
		endrow -- ending row of window to be scrolled
		endcol -- ending column of window to be scrolled
		lines -- number of lines to scroll
		attrib -- attribute to be used for blanked area

	Does int 10 function 07h to scroll down the contents of a window by
	a specified number of lines.  If the number of lines to scroll is zero,
	the entire window is blanked.
/****************************************************************************/



/****************************************************************************/
	void VideoScrollUp( int StartRow, int StartCol, int EndRow, int EndCol,
                       int Lines, int Attrib )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM

	ENTRY
		startrow -- starting row of window to be scrolled
		startcol -- starting column of window to be scrolled
		endrow -- ending row of window to be scrolled
		endcol -- ending column of window to be scrolled
		lines -- number of lines to scroll
		attrib -- attribute to be used for blanked area

	Does int 10 function 06h to scroll up the contents of a window by
	a specified number of lines.  If the number of lines to scroll is zero,
	the entire window is blanked.
/****************************************************************************/




/****************************************************************************/
	void VideoSetCursSize ( unsigned Size )

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM

	ARGUMENTS
		Size -- 	specifies size of cursor: bits 0-4 of high byte contain
					starting line for cursor and bits 0-4 of low byte contain
					ending line for cursor.

	Does INT 10 function 01h to set the size of the cursor.
/****************************************************************************/



/****************************************************************************/
	void VideoSetMode (int NewMode);
	
	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM

	VideoSetMode() attempts to set the video mode
	passed as the argument. Returns new current mode
/****************************************************************************/



/****************************************************************************/
	void VideoSetPage ( int ScrPage )

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM
	
	ARGUMENTS
		ScrPage -- screen page to set
	
	Sets active display page.
/****************************************************************************/





/****************************************************************************/
	void VideoSetRowCol ( int Row, int Column)

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM

	ARGUMENTS
		Row -- row to move cursor to
 		Column -- column to move cursor to

	Modify current cursor position.
/****************************************************************************/



/****************************************************************************/
	VideoVertDupAttr( int Row, int Col, int Attrib, int DupFactor )

	INCLUDE: BIOS_IO.H
	
	LOCATION: \BIOS\VIDEO.ASM

	ENTRY
		row -- row to start changing attribute
		col -- column to start changing attributes
		attr -- attribute to set characters to
		dupfactor -- number of characters in the line

	Changes the attributes of a vertical line of characters on the display 
	and rewrites it with the new attribute set.
/****************************************************************************/



/****************************************************************************/
	VideoVertDupCharAttr (int Row, int Col, int DupChar, int Attrib, 
								 int DupFactor)

	INCLUDE: BIOS_IO.H

	LOCATION: \BIOS\VIDEO.ASM
								 
	ENTRY
		row -- row to start writing character
		col -- column to start writing character
		dupchar -- character to be written
		attrib -- attribute of character to be written
		dupfactor -- number of characters in the line

	Writes a vertical line of characters on the display.
/****************************************************************************/


/****************************************************************************/
	int  V7VGA(void)

	INCLUDE: BIOS_IO.H

	LOCATION: \HARDWARE\VIDTEST.ASM

	EXIT: TRUE is Video 7 VGA card is detected, FALSE otherwise

	This function detects the presence of the Video 7 VGA card.
/****************************************************************************/





/***************************************************************************/
	void  WindowMove( struct WindowStruct *Wind, char *Buffer,					
							int iSaveRestore )												

	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_MOVE.C            

	ARGUMENTS:	Wind 			- Ptr to a completed window definition struc		
					Buffer 		- Ptr to data area large enough to hold the		
								  	screen contents  										
					SaveRestore - signals copy from screen to buffer or buffer	
								  	to screen													
								  	SAVE = copy from screen								
								  	RESTORE = copy to screen         					
	RETURNS:		void																			
																								


	Moves a window from the screen to a buffer if SaveRestore == SAVE       
	else it moves from the buffer to the screen.										
/***************************************************************************/




/***************************************************************************/
	void WorkAreaCls( void )																
	
	INCLUDE: WINDOW.H

	LOCATION: \WINDOW\W_CLS.C

	ARGUMENTS:	NONE																			
	
	RETURNS:		void																			

	Function to clear the work area of the screen. The work are is defined  
	as the area below the screen header and above the help prompt at the    
	bottom of the screen.                                                   
/***************************************************************************/





/************************************************************************/
	int WriteBoot( int iDrv, struct BPB *Bpb, char *szLabel )				

	INCLUDE: DISK_IO.H
	
	LOCATION: \DISK\DSK_BOOT.C
																						
	ARGUMENTS:	Bpb		- BPB to use for this disk								
					szLabel	- Disk label string or NULL for no label			
	RETURNS:		int		- OK or BIOS int 13h disk error						

	Copies the BPB structure for the current disk to a boot record and	
	then writes the boot record to the disk.										
/************************************************************************/


/***************************************************************************/
	int Xcopy( struct MULT_FILES *FileList )									
																							
	INLCUDE: COPY.H
	
	LOCATION: COPY\FILECOPY.C
	
	ARGUMENTS:	FileList 	- Array of structures which describe everything 
								  	necessary about the files.							
	RETURNS: 	void																			
	
	Functions to copy multiple files between different drives. Will prompt	
	for the disks before each file is read or written if the disk is not 	
	already inserted. Will allow the user to decide whether to continue or	
	or not if there is an error reading any of the files.
/***************************************************************************/


