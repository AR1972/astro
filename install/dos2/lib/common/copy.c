/*
 *  copy.c - Copy routine for WinDosSetup
 *  Todd Laney
 *
 *  Modification History:
 *  3/24/89  Toddla      Wrote it
 *
 *
 *  notes:
 *	we now use the LZCopy stuff so COMPRESS is NOT defined
 *	we now set the crit error handler ourselves so CHECKFLOPPY is
 *	NOT defined
 */

#include <dos.h>
#include <malloc.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <io.h>
#include <string.h>

#define COMPRESS

#ifdef COMPRESS
#include "lzcopy.h"
#else
#include "sulib.h"
#endif



int    XDosCopy;                    /* Added to allow renaming DOS files	*/
char   szDosDesName[ 25 ];          /* Added to allow renaming DOS files	*/
#define  _A_FINDALL   (_A_HIDDEN | _A_SYSTEM | _A_RDONLY)	/* DOS Upgrade	*/


char szDisks[] = "disks";

BOOL NEAR GetDiskPath(char cDisk, PSTR szPath);
#ifdef CHECK_FLOPPY
BOOL NEAR IsDiskInDrive(int iDisk);
#endif

/*
 *  global vars used by DosCopy
 */
static LPSTR    lpBuf = NULL;   // copy buffer
static int      iBuf = 0;       // usage count
static WORD     nBufSize;

#define MAXBUF  (60 * 1024)     // size of default copy buffer

void NEAR PASCAL AllocCopyBuf();
void NEAR PASCAL FreeCopyBuf();

void FAR PASCAL fartonear(LPSTR dst, LPSTR src)
{
	while (*src)
		*dst++ = *src++;

	*dst = 0;
}

/*  WORD FileCopy (szSource, szDir, fpfnCopy, WORD f)
 *
 *  This function will copy a group of files to a single destination
 *
 *  ENTRY:
 *
 *  szSourc      : pointer to a SETUP.INF section
 *  szDest       : pointer to a string containing the target DIR
 *  fpfnCopy     : callback function used to notify called of copy status
 *  fCopy        : flags
 *
 *      FC_SECTION      - szSource is a section name
 *      FC_LIST         - szSource is a pointer to a list, each item \0
 *                        terminated and the whole list \0\0 terminated
 *      FC_FILE         - szSource is a file name.
 *      FC_QUALIFIED    - szSource is a fully qualified file name.
 *      FC_DEST_QUALIFIED - szDir is fully qualified. Don't expand this.
 *      FC_DEST_QFILE   - szDir is a fully quilified path and filename.
 *
 *  NOTES:
 *      if szSource points to a string of the form '#name' the section
 *      named by 'name' will be used as the source files
 *
 *      the first field of each line in the secion is used as the name of the
 *      source file.  A file name has the following form:
 *
 *          #:name
 *
 *          #       - Disk number containing file 1-9,A-Z
 *          name    - name of the file, may be a wild card expression
 *
 *  Format for copy status function
 *
 *  BOOL FAR PASCAL CopyStatus(int msg, int n, PSTR szFile)
 *
 *      msg:
 *          COPY_ERROR          error occured while copying file(s)
 *                              n      is the DOS error number
 *                              szFile is the file that got the error
 *                              return: TRUE ok, FALSE abort copy
 *
 *          COPY_STATUS         Called each time a new file is copied
 *                              n      is the percent done
 *                              szFile is the file being copied
 *                              return: TRUE ok, FALSE abort copy
 *
 *          COPY_INSERTDISK     Please tell the user to insert a disk
 *                              n      is the disk needed ('1' - '9')
 *                              return: TRUE try again, FALSE abort copy
 *
 *          COPY_QUERYCOPY      Should this file be copied?
 *                              n      line index in SETUP.INF section (0 based)
 *                              szFile is the line from section
 *                              return: TRUE copy it, FALSE dont copy
 *
 *          COPY_START          Sent before any files are copied
 *
 *          COPY_END            Sent after all files have been copied
 *                              n   is dos error if copy failed
 *
 *
 *  EXIT: returns TRUE if successful, FALSE if failure.
 *
 */

WORD FAR PASCAL FileCopy (LPSTR szSource, PSTR szDir, FPFNCOPY fpfnCopy, WORD fCopy)
{
    int       err = ERROR_OK;
    char      szFile[MAXPATHLEN];
    char      szPath[MAXPATHLEN+20];
    char      szLogSrc[MAXPATHLEN+20];
    char      szSrcBase[15];
    char      szSrc[MAXPATHLEN+20];
    char      szErrFile[MAXPATHLEN+20];
    LPSTR far *pFileList;
    LPSTR far *pFileListBegin;
    LPSTR     pFile;
    LPSTR     pFileBegin;
    BOOL      f;
    BOOL      fDoCopy;
    int       n = 0;
    int       nDisk;
    char      cDisk;
    int       cntFiles = 0;
    PINF      pinf;

    #define CALLBACK(msg,n,pFile) \
        (fpfnCopy ? ((*fpfnCopy)((WORD)(msg),(int)(n),(LPSTR)(pFile))) : FC_IGNORE)

    if (!szSource || !*szSource || !szDir || !*szDir)
        return FALSE;

#ifndef COMPRESS
    AllocCopyBuf();
#endif

    /*
    if ( fCopy & (FC_DEST_QUALIFIED | FC_DEST_QFILE) )
     *  fix up the drive in the destination
     */


    if ( fCopy & FC_DEST_QUALIFIED )
       strcpy(szPath,szDir);
//    else
//       ExpandFileName(szDir,szPath);

    fCopy &= ~FC_DEST_QUALIFIED;

    if (szSource[0] == '#' && fCopy == FC_FILE)
    {
        fCopy = FC_SECTION;
        ++szSource;
    }

    switch (fCopy)
    {
        case FC_LSTPTR:
            pFileList = pFileListBegin = (LPSTR far *)szSource;
            pFileBegin = *pFileList;

            while ( pFileList[n] ) {
               if ( *pFileList[n] )
                  ++cntFiles;
               ++n;
            }

            break;

        case FC_SECTION:
	    {
	    char buf[40];

	    fartonear(buf, szSource);
//            szSource = infFindSection(NULL,buf);
            if (szSource == NULL)
                goto exit;

	    fCopy = FC_LIST;
	    }
            // fall through to FC_LIST

        case FC_LIST:
            pFileBegin = szSource;
//            cntFiles = infLineCount(szSource);
            break;

        case FC_FILE:
        case FC_QUALIFIED:
        default:
            pFileBegin = szSource;
            cntFiles = 1;
    }

    /*
     * Does the destination directory exist? if not create it.
     */
    if (!DosValidDir(szPath)) {

        err = DosMkDir(szPath);

	// oh no! this is bad

        if (err != ERROR_OK) {
            CALLBACK(COPY_ERROR,err,szPath);
            goto exit;
        }
    }

    /*
     *  walk all files in the list and call DosCopy ....
     *
     *  NOTES:
     *      we must walk file list sorted by disk number.
     *      we should use the disk that is currently inserted.
     *      we should do a find first/find next on the files????
     *      we need to check for errors.
     *      we need to ask the user to insert disk in drive.
     *
     */
    CALLBACK(COPY_START,0,NULL);

    for (nDisk = 1; cntFiles > 0; nDisk++) {

        cDisk      = CHDISK(nDisk);
        pFileList  = pFileListBegin;
        pFile      = pFileBegin;
        n          = 0;

        while (pFile) {

            /*
             *  should we copy this file?
             *  copy the files in disk order.
             */
            fDoCopy = pFile[1] == ':' && cDisk == UP_CASE(pFile[0]) ||
                      pFile[1] != ':' && nDisk == 1 && *pFile ||
                      fCopy == FC_QUALIFIED;

            if (fDoCopy)
                cntFiles--;         // done with a file. decrement count.

            if (fDoCopy && CALLBACK(COPY_QUERYCOPY,n,pFile)) {

                if (CALLBACK(COPY_STATUS, 0, pFile) == FC_ABORT) {
		    err = ERROR_NOFILES;
                    goto exit;
		}

		// now we convert logical dest into a physical (unless FC_QUALIFIED)

//                infParseField(pFile, 1, szLogSrc);	 // logical source
	              fartonear(szLogSrc,pFile);

                if ( fCopy != FC_QUALIFIED )
                   strcpy(szSrc,szLogSrc);
//                   ExpandFileName(szLogSrc, szSrc); // full physical source
                else
                   strcpy(szSrc,szLogSrc);

tryagain:   
                // Call low level copy command

                err = DosCopy(szSrc, szPath);
      
                if (err != ERROR_OK) {

		    strcpy(szSrcBase, FileName(szSrc));	// save base name

                    if (err == ERROR_FILENOTFOUND || err == ERROR_PATHNOTFOUND) {

                        // isolate the path

                        StripPathName(szSrc);

		   	// now try to get a new path in szSrc

                   	switch (CALLBACK(COPY_INSERTDISK, szLogSrc[0], szSrc)) {
	           	case FC_RETRY:
		   		catpath(szSrc, szSrcBase);	// add the file back on
		   		goto tryagain;			// and try again...

                   	case FC_ABORT:
                   		goto exit;

	           	case FC_IGNORE:
        	        	break;
                   	}

                    }

		    // ERROR situation
		    //
		    // this may be a real error or something like
		    // a share violation on a network.

          strcpy(szSrc,szLogSrc);

//		    ExpandFileName(szLogSrc, szSrc);	// full physical source

		    // if it is a write error report the destination file
		    // otherwise report with the source file

		    switch (err) {
		       case ERROR_WRITE:
		    	   strcpy(szErrFile, szPath);
			      catpath(szErrFile, szSrcBase);
		    	   break;

		    default:
		    	strcpy(szErrFile, szSrc);
		    }

                    switch (CALLBACK(COPY_ERROR, err, szErrFile)) {

                    case FC_RETRY:
                            goto tryagain;
    
                    case FC_ABORT:
                            goto exit;

                    case FC_IGNORE:
                            break;
                    }
                }

                if (CALLBACK(COPY_STATUS,100,pFile) == FC_ABORT)
                    goto exit;
            }

            /*
             * Move on to next file in the list
             */
            n++;
            if (fCopy == FC_LSTPTR)
                pFile = *(++pFileList);
//	         else if (fCopy == FC_LIST)
//                pFile = infNextLine(pFile);
            else
                pFile = NULL;
        }
    }

    err = ERROR_OK;

exit:
    CALLBACK(COPY_END,err,NULL);
#ifndef COMPRESS
    FreeCopyBuf();
#endif
    return err;

    #undef CALLBACK
}

#ifndef COMPRESS

/*  AllocCopyBuf()
 *
 *  allocate a buffer for DosCopy to use
 *
 */
void NEAR PASCAL AllocCopyBuf()
{
    if (iBuf++ == 0)
    {
        nBufSize = MAXBUF;
        for(;;)
        {
            lpBuf = FALLOC(nBufSize);
            if (lpBuf || nBufSize == 1)
                break;
            nBufSize /= 2;
        }
        if (lpBuf == NULL)
            iBuf--;
    }
}

/*  FreeCopyBuf()
 *
 *  free copy buffer, if its use count is zero
 *
 */
void NEAR PASCAL FreeCopyBuf()
{
    if (iBuf > 0 && --iBuf == 0 && lpBuf)
    {
        FFREE(lpBuf);
    }
}
#endif

PSTR GetExtension(PSTR szFile)
{
	PSTR ptr;

	for (ptr = szFile; *ptr && *ptr != '.'; ptr++);

	if (*ptr != '.')
		return NULL;
	else
		return ptr+1;

}

BOOL GetCompressedName(PSTR szComp, PSTR szSrc)
{
	PSTR ptr;

	strcpy(szComp, szSrc);

	ptr = GetExtension(szComp);

	if (ptr )
	{
		szComp[strlen(szComp)-1] = '_';
		return TRUE;
	}
#if	0	/* Changed for dos/windows merge */
	if (ptr && !strcmpi(ptr, "sys"))
	{
		szComp[strlen(szComp)-1] = '$';
		return TRUE;
	}
#endif

	return FALSE;
}

/*  DosCopy(PSTR szSrc, PSTR szPath)
 *
 *  Copy the file specifed by szSrc to the drive and directory
 *  specifed by szPath
 *
 *  ENTRY:
 *      szSrc   - File name to copy from
 *      szPath  - directory to copy to
 *
 *  RETURNS:
 *      0 - no error, else dos error code
 *
 */
int NEAR DosCopy(PSTR szSrc, PSTR szPath)
{
    FCB         fcb;
    WORD        size;
    int         fhSrc,fhDst;
    char        szFile[MAXPATHLEN+20];
    char        szComp[MAXPATHLEN+20];
    int         f = ERROR_OK;
    unsigned    date;
    unsigned    time;
    long	l;
    BOOL	bCompressedName;
    

#ifdef DEBUG
    if (fDontCopy)
        return ERROR_OK;

//    if (infGetProfileString(NULL,"setup","copy",szFile) && szFile[0] == 'f')
//        return ERROR_OK;
#endif

#ifndef COMPRESS
    AllocCopyBuf();

    if (!lpBuf)
        return ERROR_NOMEMORY;
#endif

#ifdef CHECK_FLOPPY
    if (!IsDiskInDrive(szSrc[0]))
    {
        f = ERROR_FILENOTFOUND;
        goto errfree;
    }
#endif


    // allows both sy$ and sys on the disks

    if (GetCompressedName(szComp, szSrc) &&
    	DosFindFirst(&fcb, szComp, XDosCopy ? _A_FINDALL : ATTR_FILES)) {

    	bCompressedName = TRUE;

    } else {

        bCompressedName = FALSE;

    	if (!DosFindFirst(&fcb, szSrc, XDosCopy ? _A_FINDALL : ATTR_FILES)) {
		f = ERROR_FILENOTFOUND;
		goto errfree;
	}
    }

    /*
     * copy every file that matches the file pattern passed in.
     */
    do
    {
        /*
         * create the source file name from the source path and the file
         * name that DosFindFirst/Next found
         */
        strcpy(szFile,szSrc);
        StripPathName(szFile);
        catpath(szFile,fcb.szName);

        fhSrc = FOPEN(szFile);

        if (fhSrc == -1)
        {
            f = FERROR();
            goto errfree;
        }

//#ifndef COMPRESS
        /* Save date of opened file */

        if (_dos_getftime(fhSrc,&date,&time))
           goto errclose1;
//#endif
        /*
         * create the destination file name from the dest path and the file
         * name that DosFindFirst/Next found
        */

        strcpy( szFile, szPath );

          /* bIsUpgrade - johnhe 05-25-91
           * If XDosCopy != FALSE then we are doing a DOS copy and
           * we have to get the destination name from the global
           * array szDosDesName.
          */
             
        if ( XDosCopy )
        {
           struct find_t    Info;

           catpath( szFile, szDosDesName );

           _dos_setfileattr( szFile, 0 );

                /* First see if the file exists and if it does we need to */
                /* open it without truncating. This is necessary so that  */
                /* IO.SYS/MSDOS.SYS will truly be written over top of the */
                /* original files so that UNINSTALL can restore the files */

           if ( _dos_findfirst( szFile, _A_FINDALL, &Info ) == 0 )
           {
              if ( (FERROR() = _dos_open( szFile, O_RDWR, &fhDst )) != 0 )
	              fhDst = -1;
           }
           else if ( (FERROR() = _dos_creat( szFile, 0, &fhDst )) != 0 )
              fhDst = -1;
        }
        else
        {
              // don't support wildcards for compressed files
           if (bCompressedName)
              catpath( szFile, FileName(szSrc) );
           else
              catpath( szFile, fcb.szName );	// used name from fcb
           fhDst = FCREATE( szFile );
        }

        if (fhDst == -1)
        {
           f = FERROR();
           goto errclose1;
        }

#ifdef COMPRESS

	// translate LZERROR_ returns (all < 0) to DOS errors

        if ((l = DOSLZCopy(fhSrc, fhDst)) < 0) {

		// was this a dos error?

		if (FERROR() != 0) {
			f = FERROR();
		} else {

			// translate LZ error codes to DOS errors

			switch ((int)l) {
			case LZERROR_BADINHANDLE:
			case LZERROR_READ:
				f = ERROR_READ;
				break;

			case LZERROR_BADOUTHANDLE:
			case LZERROR_WRITE:
				f = ERROR_WRITE;
				break;

			case LZERROR_GLOBALLOC:
			case LZERROR_GLOBLOCK:
				f = ERROR_NOMEMORY;
				break;
			}
		}
	}
#else
        while (size = FREAD(fhSrc,lpBuf,nBufSize))
        {
            if (FWRITE(fhDst,lpBuf,size) != size)
            {
                /* write error? */
                f = FERROR();
                if (f == ERROR_OK)
                    f = ERROR_WRITE;
                goto errclose;
            }
        }

        /* Restore date of written file */
        _dos_setftime(fhDst,date,time);
#endif
		if ( f == 0 )
      {
         chsize( fhDst, lseek( fhDst, 0L, SEEK_CUR ) );
         _dos_setftime(fhDst,date,time);
      }
      else
         /*
          *  We've had some kind of copy error here and we need to delete
          *  the zero length file we have created at the destination.
          *  MC Bugfix #178 for UPG/JANUG 5/21/92
          */
         remove(szFile);

errclose:
        FCLOSE(fhDst);
errclose1:
        FCLOSE(fhSrc);

    }   while ( f == ERROR_OK && DosFindNext(&fcb) );

errfree:

#ifndef COMPRESS
    FreeCopyBuf();
#endif

    return f;
}

#if 0

/*  BOOL FAR PASCAL ExpandFileName(PSTR szFile, PSTR szPath)
 *
 *  This function will retrive the full path name for a file
 *  it will expand, logical disk letters to pyshical ones
 *  will use current disk and directory if non specifed.
 *
 *  if the drive specifed is 0-9, it will expand the drive into a
 *  full pathname using GetDiskPath()
 *
 *  IE  0:system ==>  c:windows\system
 *      1:foo.txt     a:\foo.txt
 *
 *  ENTRY:
 *
 *  szFile       : File name to expandwhat disk to find
 *  szPath       : buffer to hold full file name
 *
 */
BOOL FAR PASCAL ExpandFileName(PSTR szFile, PSTR szPath)
{
    char    szBuf[MAXPATHLEN*2];

    if (szFile[1] == ':' && GetDiskPath(szFile[0],szBuf))
    {
        strcpy(szPath,szBuf);
        if (szFile[2])
            catpath(szPath,szFile + 2);
    }
    else
    {
        strcpy(szPath,szFile);
    }
    return TRUE;
}

/*  BOOL GetDiskPath(char cDisk, szPath)
 *
 *  This function will retrive the full path name for a logical disk
 *  the code reads the [disks] section of SETUP.INF and looks for
 *  n = path where n is the disk char.  NOTE the disk '0' defaults to
 *  the root windows directory.
 *
 *  ENTRY:
 *
 *  cDisk        : what disk to find 0-9,A-Z
 *  szPath       : buffer to hold disk path
 *
 */
BOOL NEAR GetDiskPath(char cDisk, PSTR szPath)
{
    char    ach[2];
    char    szBuf[MAXPATHLEN];

    if (cDisk == '0')
    {
        /*
         * return the windows setup directory
         */
        strcpy(szPath,szSetupPath);
        return TRUE;
    }

    /*
     * now look in the [disks] section for a full path name
     */
    ach[0] = cDisk;
    ach[1] = 0;
//    if ( !infGetProfileString(NULL,szDisks,ach,szPath) )
//       return FALSE;
//    infParseField(szPath,1,szPath);
    /*
     *  is the path relative? is so prepend the szDiskPath
     */
    if (szPath[0] == '.' || szPath[0] == 0)
    {
        strcpy(szBuf,szDiskPath);
        catpath(szBuf,szPath);
        strcpy(szPath,szBuf);
    }
    return TRUE;
}

#endif

#ifdef CHECK_FLOPPY

/*  BOOL IsDiskInDrive(char cDisk)
 *
 *  Is the specifed disk in the drive
 *
 *  ENTRY:
 *
 *  cDisk        : what disk required to be in the drive (logical)
 *
 *  return TRUE if the specifed disk is in the drive
 *         FALSE if the wrong disk is in the drive or disk error
 *
 */
BOOL NEAR IsDiskInDrive(int iDisk)
{

    if ((iDisk  >= 'A' && iDisk <= 'Z') || 
    	(iDisk  >= 'a' && iDisk <= 'z')) {

	    if (DosRemoveable(iDisk)) {

	        if (!IsValidDiskette(iDisk))
        		return FALSE;
	    }


	    return TRUE;
    }

    return TRUE;	// for non drive letters assume a path
    			// and thus always in.
}

#define CBSECTORSIZE	512
#define INT13_READ	2

/*--------------------------------------------------------------------------
									    
  IsValidDiskette() - 						    
									    
--------------------------------------------------------------------------*/

BOOL NEAR IsValidDiskette(int iDrive)
{
  char	    buf[CBSECTORSIZE];

  iDrive |= 0x0020;	// make lower case

  iDrive -= 'a';	// A = 0, B = 1, etc. for BIOS stuff

  return MyReadWriteSector(buf, INT13_READ, iDrive, 0, 0, 1);
}

#endif 

void FAR PASCAL catpath(PSTR path, PSTR sz)
{
    //
    // Remove any drive letters from the directory to append
    //
    if ( sz[1] == ':' )
       sz+=2;

    //
    // Remove any current directories ".\" from directory to append
    //
    while (sz[0] == '.' && SLASH(sz[1]))
		  sz+=2;

	//
	// Remove leading slashes.
	//
	while (SLASH(*sz))
		sz++;

    //
    // Dont append a NULL string or a single "."
    //
    if (*sz && !(sz[0] == '.' && sz[1] == 0))
    {
       if ( (!SLASH(path[strlen(path)-1])) && ((path[strlen(path)-1]) != ':') )
          strcat(path,CHSEPSTR);
       strcat(path,sz);
    }
}


PSTR FAR PASCAL FileName(PSTR szPath)
{
    PSTR   sz;

    for (sz=szPath; *sz; sz++)
        ;
    for (; sz>=szPath && !SLASH(*sz) && *sz!=':'; sz--)
        ;
    return ++sz;
}

PSTR FAR PASCAL StripPathName(PSTR szPath)
{
    PSTR   sz;

    sz = FileName(szPath);

    if (sz > szPath+1 && SLASH(sz[-1]) && sz[-2] != ':')
        sz--;

    *sz = 0;
    return szPath;
}

