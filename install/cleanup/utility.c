/***************************************************************************/
/* 																								*/
/*	UTILITY.C																					*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/***************************************************************************/

#include <direct.h>
#include <disk_io.h>
#include <dos.h>
#include <fcntl.h>
#include <message.h>
#include <stdio.h>
#include <string.h>
#include <strlib.h>
#include <window.h>
#include <alias.h>

#include "global.h"



/*****************************************************************************
 *	void AddExtension ( char *path, char *extension )
 *
 *	Takes a string with a path and changes the extension of the file in the
 *	path ( in the string only, not on disk.)
 *
 *	ENTRY
 *		path -- path string to be changed
 *		extension -- new extension to be added, if NULL the functions returns
 *						 without doing anything
 *	EXIT
 *		returns nothing
 *
 ****************************************************************************/

void AddExtension ( char *path, char *extension )
{

	if ( FileExtension ( path ) == NULL )
		strcat (path, ".");

	strcpy ( FileExtension ( path ), extension );

}




/*****************************************************************************	
 *		AddPath -- add extension to a path string
 *
 *		Adds an extension to a path string.  If the path isn't the root 
 *		directory then a '\' is added to seperate the path and extension.
 *
 *		void AddPath (char *path, char *extension)	
 *
 *		ENTRY
 *			path -- string of path to be appended to
 *			extension -- string of extension to append onto path
 *		EXIT
 *			path -- path has extension appended to it
 *
 *		WARNINGS
 *			Assumes that there is enough free memory at the end of the path
 *			string to accomodate the extension.		
 *	
 ****************************************************************************/





void AddPath (char *path, char *extension)
{
	
#ifdef DBCS
	if (path [strlen (path) - 1] != '\\' || CheckDBCSTailByte(path,&path [strlen(path)-1]))
#else
	if (path [strlen (path) - 1] != '\\')
#endif
		strcat (path, "\\");
	strcat (path, extension);
}



#if 0

/*****************************************************************************
 *	int CheckSum ( char *path, int *checksum );
 *
 *	Does all checksum by adding up all the bytes in a file one word at a time
 *	into an integer and ignoring carries.  If the file is of odd length the 
 * first byte is treated as a word with a clear low byte and added into
 * the checksum.
 *
 *	ENTRY
 *		path -- string containing the complete path of the file to checksum
 *		checksum - ptr to int in which to put checksum
 *	EXIT
 *		0 if succesful, !0 otherwise
 *
 *	GLOBALS
 *		GlobalBuf is used to read in the file.
 *
 *	WARNING
 *		checksum relies on GlobalBufSize being an even number
 *
 ****************************************************************************/
 

int CheckSum ( char *path, int *checksum )
{
	int file, count2;
	int status = 0;
	int sum = 0;
	long size = FileSize ( path );
	unsigned	readsize;
	long	count1;

	
		/* If we have an odd file size, add in the first byte separately.*/


	if (_dos_open ( path, O_RDONLY, &file ))
		return 0;
		
	if ( size & (long) 0x01 )
	{
		status = _dos_read ( file, (char *) &sum, (unsigned ) 1, &count2 );
	 	size --;
	}

	for (count1 = 0l; count1 <  size && !status; count1 += GlobalBufSize)
	{
	 /*	readsize = (unsigned ) (MIN ((long) GlobalBufSize, size - count1));*/
	
		status = _dos_read ( file, GlobalBuf, GlobalBufSize , &readsize);
		
	   sum += CheckSumBuffer ( (int *) GlobalBuf, readsize / 2 );
		
  	}

	_dos_close ( file );

	*checksum =	sum;

	return status;

}

/*****************************************************************************
 *	int	CheckSumBuffer ( int *buffer, unsigned size )
 *
 *	Adds the contents of a buffer one word at a time to an integer and
 *	returns the result.
 *
 *	ENTRY
 *		buffer - pointer to buffer to sum up
 *		size - size of buffer in words
 *	EXIT
 *		sum of buffer
 *
 ****************************************************************************/

int CheckSumBuffer ( int *buffer, unsigned size )
{
	register int sum = 0;
	register unsigned count;
	int *bufcount = buffer;

	for ( count = 0; count < size; count++, sum += *bufcount++ );

	return sum;

}

#endif


/*****************************************************************************
 *
 *	char *FileExtension ( char *path )
 *
 *	FileExtension returns a pointer to the extension of a file.  If the
 *	file has no extension it returns NULL.
 *
 *	ENTRY
 *		path -- path of file
 *	EXIT
 *		ptr to extension of file or null
 *
 ****************************************************************************/
																								 
char *FileExtension ( char *path )
{
	char *extension = (char *) strchr ( ParseFileName ( path ), '.' );

	if (extension == NULL)
		return NULL;
	
	return ++extension;
}

#if 0

/*****************************************************************************
 *	long FileSize ( char *path )
 *
 *	Uses the find_t struct set by _dos_findfirst to determine the size of a
 *	specified file.
 *
 *	ENTRY
 *		path -- path of file whose size is to be checked.
 *	EXIT
 *		size of file 0 if ther is an error openning the file
 *
 ****************************************************************************/

long FileSize ( char *path ) 
{
	struct find_t fileinfo;

	if (_dos_findfirst ( path, _A_ALLFILES, &fileinfo ))
		return 0l;

	return fileinfo.size;
}



/*****************************************************************************
 *	int FileNameCompare ( char *file1, char *file2 )
 *
 *	Compares two file names ( ignoring their extensions) and returns 0 if
 *	they are the same or !0 otherwise.
 *
 *	ENTRY
 *		file1 -- string of first file to compare
 *		file2 -- string of second file to compare
 *	EXIT
 *		0 if the names are same, !0 otherwise
 *	
 ****************************************************************************/	
	
int FileNameCompare ( char *file1, char *file2 )
{
	int	result = 0 ;

 	result = *file1 - *file2;
	
	while ( (*file1 != '.')  && (*file2	!= '.') && (result == 0 ))
		result = *(++file1) - *(++file2);

	return *file1 - *file2;

}

/************************************************************************/
/*	Fills in a an array with all valid drive letters which are not			*/
/* floppy disk supported by the ROM BIOS.											*/
/*																								*/
/*	void GetDrvList( char *DrvList )													*/
/*																								*/
/*	ARGUMENTS:	DrvList	- Array of chars to be filled in. (Max 27 chars)*/
/*	RETURNS:		void																		*/
/*																								*/
/************************************************************************/

void GetDrvList( char *DrvList )
{
	int		CurrentDrv;
	int		scratch;
	int		ThisDrv;
	int		TmpDrv;
	int		i;
	
								/* Find first hard drive number 
	ThisDrv = GetNumberOfDrives() + 1;
	if ( ThisDrv < 3 )	 */
		ThisDrv  = 3;

	_dos_getdrive( &CurrentDrv );

	for( i = 0; i <= MAX_H_DRIVES; ThisDrv++ )
	{
		_dos_setdrive( ThisDrv, &scratch );			/* Try to set to next drive*/
		_dos_getdrive( &TmpDrv );		/* See if successfull			*/
		if ( TmpDrv != ThisDrv )
			break;								/* No more hard drive letters	*/
		else if ( IsRemoveable( (char ) ThisDrv ) != 1 &&
					 IsLocalDrive( (char ) ThisDrv ) != 0 )
			DrvList[i++] = (char ) ThisDrv - 1;
		else
			; 										/* Don't add to the list */

	}
	DrvList[i] = 0;							/* Mark end of list				*/

	_dos_setdrive( CurrentDrv, &scratch );		/* Restore original setting	*/
}

/*****************************************************************************
 *	int GetPacket ( FILE_PACKET *apacket, char *path )
 *
 *	Gets the packet on a file specified by path.  If the file has no packet
 * it does nothing and returns false.
 *
 *	ENTRY
 *		apacket -- ptr to packet to read info into
 *		path -- path of paket from which to get the packet
 *	EXIT
 *		FALSE if the file has no packet, TRUE otherwise
 *
 ****************************************************************************/
 
int GetPacket ( FILE_PACKET *apacket, char *path )
{
	int filehandle;
	unsigned scratch;

	if ( _dos_open ( path, O_RDONLY, &filehandle ))
		return FALSE;

	if ((_dos_seek (filehandle, - (long) sizeof (FILE_PACKET), FROM_END ) == -1) ||
		 (_dos_read (filehandle, (char *)apacket,sizeof (FILE_PACKET),&scratch)) ||
		 (_dos_close ( filehandle )))
		  return FALSE;
	
	return !(strcmp (SIGNATURE, apacket->Signature) );

}

/*****************************************************************************
 *	char *MakeBufCString ( char *buffer )
 *
 *	Takes a pointer to a buffer scans until it ecounters a CR or LF and converts
 *	it to an EOL ( \0 ) and returns a pointer to the original buffer (which
 *	now points to a newly exctracted C string.  The original pointer to
 *	the buffer will point to the location immediately after the new string.
 *	If the first character(s) of the buffer are CR's or LF's they will be skipped
 *	over, having the effect of skipping blank lines.	
 *
 *
 *	ENTRY
 *		buffer -- ptr to a ptr to a buffer from which to make strings
 * EXIT
 *		returns -- location in buffer immediately following newly created C string.
 *		buffer -- now points to location immediately following newly extracted
 *					 string.
 *
 ****************************************************************************/

char *MakeBufCString ( char **buffer )
{
	char	*base;
		
			
	while (1)
	{
			/*Skip over leading \n's and spaces.*/

		while ( (**buffer == CR) || (**buffer == LF) || (**buffer == SPACE ) )
			(*buffer)++; 

			/*Skip over comments.*/
	
		if (**buffer == ';')
			while (**buffer != CR)
				(*buffer)++;
		else
			break;
	}

	base = *buffer;

	while ( (**buffer != CR) && (**buffer != LF) )
		(*buffer)++;

	*((*buffer)++) = EOL;

	return base;
}

/*****************************************************************************
 *	void MakeOptionString ( char *target, char *source, int upgrade )
 *
 *	Prefixes a string with either "UPGRADE" or "DO NOT UPGRADE" and copies
 *	the new string to a buffer.  
 *
 *	ENTRY
 *		source -- source string 
 *		target -- buffer to copy new string to
 *		upgrade -- if !0 add the "UPGRADE" prefix otherwise add the
 *					  "DO NOT UPGRADE" prefix.
 *	EXIT
 *		returns nothing
 *
 ****************************************************************************/

void MakeOptionString ( char *target, char *source, int upgrade )
{
	char *apszText[3];
	
	GetMessage ( apszText, UPGRADE_PREFIXES );

	strcpy ( target, apszText[ ( upgrade != 0 )] );

	strcat ( target, source );

	GetMessage ( apszText, UPGRADE_SUFFIXES );

	strcat ( target, apszText[ ( upgrade != 0 )] );
	
}

#endif

/***************************************************************************** 	
 *	void MyPuts (char *str)
 *
 *	Writes a string to stdout.
 *		
 *	ENTRY
 *	 str -- the string to write
 *	EXIT
 *	returns nothing
 *
 ****************************************************************************/



void MyPuts (char *str)
{
	for (; *str; putchar (*str++));
	putchar ( CR );
}





/*****************************************************************************
 *	void RemoveDir ( char *path )
 *
 *	Removes all files and directories in a directory and the directory itself.  
 *	If RemoveDir is called to delete the root it will fail.
 *
 *	ENTRY
 *	 	path -- path of the directory to delete
 *	EXIT
 *		nothing
 *
 ****************************************************************************/



void RemoveDir ( char *path )
{
	struct find_t fileinfo;

	/* If this is the root delete it under no circumstances.*/

	if ( strlen ( path ) < 4 )
		return;

	AddPath ( path, "*.*" );

	/*First skip "." and ".." directories.*/

	_dos_findfirst ( path, _A_NORMAL | _A_HIDDEN | _A_SUBDIR, &fileinfo );
 	_dos_findnext ( &fileinfo );

	if ( !_dos_findnext (&fileinfo))
        {
		do 
		{
			RemovePath ( path );
			AddPath ( path, fileinfo.name );
		  	  
			if ( IS_DIR & fileinfo.attrib )
				RemoveDir ( path );
			else
			{
				/*Clear read only.*/
				_dos_setfileattr ( path, 0 );
                                DisplayFileStatus ( ParseFileName ( path ), DELETE );
				remove ( path );
			}
		} while (!_dos_findnext ( &fileinfo ) );
                DisplayFileStatus ( "", CLEAR );
        }
	RemovePath ( path );
	rmdir ( path );
}



/*****************************************************************************	
 *		RemovePath -- removes tail of path
 *																									  
 *		Removes the tail file name/subdirectory from a path string.
 *
 *		void removePath (char *path)
 *
 *		ENTRY
 *			path -- string of path to be shortened
 *		EXIT
 *			path -- tail is removed
 *
 ****************************************************************************/

						



void RemovePath (char *path) {
	char 	*count;

#ifdef DBCS
	for (count = &path[strlen (path) - 1]; *count != '\\' || CheckDBCSTailByte(path,count); count --);
#else
	for (count = &path[strlen (path) - 1]; *count != '\\'; count --);
#endif
	if ((*(count - 1) == ':') || (count == path ))
		count++;	  /*If root directory then don't remove "\" */
	*count = 0;
}
 


