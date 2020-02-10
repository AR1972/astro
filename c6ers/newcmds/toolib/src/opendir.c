#define INCL_BASE
#include <os2.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>
#include <assert.h>
#include "..\h\dirent.h"

struct DirBuf				// The real DIR, hidden from the user.
	{				// Hide OS/2 include files
	HDIR hdir;			// OS/2 directory handle
	char *szPath;			// Path to directory name 
	unsigned int bInvalid: 1;	// At EOF on directory
	unsigned int bRewind: 1;	// Rewind on next read.
	char *szFNCur;			// pointer to current file name
	USHORT usSearchCount;		// number of filenames buffered
	USHORT cff2;			// number of FILFINDBUF2 structures
	FILEFINDBUF2 aff2[1];		// variable size array (cff2 #)
	};

static USHORT usMyDFF2(struct DirBuf *);
static USHORT usMyDFN(struct DirBuf *);
static USHORT usDFToPosix(struct DirBuf *, USHORT);
static struct DirBuf *pdbMake(char *);
void DosErrToErrno(USHORT);

// Default number of FILEFINDBUF2 buffers.
// User settable before opendir() calls.
USHORT _cff2Bufs = max((2 * BUFSIZ / sizeof(FILEFINDBUF2)), 1);

// opendir() -- POSIX 1003.1 compliant routine.
//	Open a directory stream corresponding to the directory
//	named by the argment.  Return a pointer to the stream
//	of type DIR *, or NULL on error.

DIR *opendir(char *szDirName)
	{
	enum {enBadArg, enAlloc, enFF2, enGood} enErr;
	struct DirBuf *pdb = NULL;

	if (NULL == szDirName)
		enErr = enBadArg;
	else if (NULL == (pdb = pdbMake(szDirName)))
		enErr = enAlloc;
	else if (usMyDFF2(pdb))
		enErr = enFF2;
	else
		enErr = enGood;
	switch (enErr)
		{
		case enFF2:
			free((char *) pdb);
		default:
		case enAlloc:
			pdb = NULL;
			break;
		case enBadArg:
			errno = EINVAL;
			pdb = NULL;
			break;
		case enGood:
			break;
		};
	return (DIR *) pdb;
	}

// readdir() -- POSIX 1003.1 compliant routine.
// 	read a file name from a directory stream.
//	return NULL if error or EOF, or a pointer 
//	to a struct dirent if not.
//	Special handling for rewinddir(), EOF, and
//	filling th buffer with DOSFINDNEXT().

struct dirent *readdir(DIR *pd)
	{
	struct DirBuf *pdb = (struct DirBuf *) pd;
	struct dirent *pde = NULL;

	if (NULL == pdb)
		{
		errno = EBADF;
		pde = NULL;
		}
	else if (pdb->bRewind && ((pdb->bRewind = 0), usMyDFF2(pdb)))
		pde = NULL;
	else if (pdb->bInvalid)
		pde = NULL;
	else if (NULL != pdb->szFNCur)
		{
		pde = (struct dirent *) pdb->szFNCur;
		if (--(pdb->usSearchCount) == 0)
			pdb->szFNCur = NULL;
		else
			pdb->szFNCur = ((FILEFINDBUF2 *) (pdb->szFNCur + 1 +
					strlen(pdb->szFNCur)))->achName;
		}
	else if (usMyDFN(pdb))
		pde = NULL;
	else 
		pde = readdir(pd);
	return pde;
	}

// closedir() -- POSIX 1003.1 compliant routine.
//	Close a directory stream and free the memory allocated to 
//	it.  This might want to return an error is the szPath field
//	of the DirBuf structure is empty -- that's an internal error.

int closedir(DIR *pd)
	{
	struct DirBuf *pdb = (struct DirBuf *) pd;
	USHORT usDFCRet;
	int iRetVal = -1;

	if (NULL == pdb)
		{
		errno = EBADF;
		return -1;
		}
	if (usDFCRet = DosFindClose(pdb->hdir))
		{
		DosErrToErrno(usDFCRet);
		iRetVal = -1;
		}
	else
		iRetVal = 0;
	if (NULL != pdb->szPath)
		free(pdb->szPath);
	free((char *) pd);
	return iRetVal;
	}

// rewinddir() -- POSIX 1003.1 compliant routine.
//	Rewind a directory stream to the beginning. 
//	Set the rewind flag, and clear the EOF flag.
//	This is to postpone errors on the DosFindFirst2()
//	until readdir(), since this is specified as a 
//	function of type void.

void rewinddir(DIR *pd)
	{
	struct DirBuf *pdb = (struct DirBuf *) pd;

	if (NULL != pdb)
		{
		pdb->bRewind = 1;
		pdb->bInvalid = 0;
		}
	}

// usMyDFF2 -- wrapper for DosFindFirst2() for POSIX functions.
//	Pass in the right arguments, and map the return code
//	through the C runtime error mapping functions.
//	Always try to squeeze as many names in the buffer as 
//	will fit.

static USHORT usMyDFF2(struct DirBuf *pdb)
	{
	assert(NULL != pdb);

	pdb->usSearchCount = USHRT_MAX;
	return usDFToPosix(pdb, DosFindFirst2((PSZ) (pdb->szPath),
					(PHDIR) &(pdb->hdir),
					FILE_NORMAL | FILE_READONLY |
					FILE_HIDDEN | FILE_SYSTEM |
					FILE_DIRECTORY | FILE_ARCHIVED,
					(PVOID) (pdb->aff2),
					sizeof(FILEFINDBUF2) * pdb->cff2,
					(PUSHORT) &(pdb->usSearchCount),
					FIL_QUERYEASIZE, (ULONG) 0L));
	}

// usMyDFN -- wrapper for DosFindNext() for POSIX functions.
//	Pass in the right arguments, and map the return code
//	through the C runtime error mapping functions.
//	Always try to squeeze as many names in the buffer as 
//	will fit.

static USHORT usMyDFN(struct DirBuf *pdb)
	{
	assert(NULL != pdb);

	pdb->usSearchCount = USHRT_MAX;
	return usDFToPosix(pdb, DosFindNext(pdb->hdir,
					(PFILEFINDBUF) (pdb->aff2),
					sizeof(FILEFINDBUF2) * pdb->cff2,
					(PUSHORT) &(pdb->usSearchCount)));
	}

// usDFToPosix -- map DosFind?() functions to the data
//	structures used to implement the POSIX layer.
//	Map the return code, set the EOF flag, 
//	set the pointer to the filename in the structure.

static USHORT usDFToPosix(struct DirBuf *pdb, USHORT usDFRet)
	{
	assert(NULL != pdb);

	switch (usDFRet)
		{
		default:
			DosErrToErrno(usDFRet);
			pdb->szFNCur = NULL;
			break;
		case ERROR_NO_MORE_FILES:
			pdb->bInvalid = 1;
			pdb->szFNCur = NULL;
			usDFRet = 0;
			break;
		case 0:
			pdb->szFNCur = pdb->aff2[0].achName;
			break;
		};
	return usDFRet;
	}

// pdbMake -- Create and initialize the DirBuf structure to
//	support the POSIX layer for opendir() etc.
//	Clear the flags, make a buffer of the size determined
//	by the global _cff2Bufs, record the size (in case 
//	the global value is changed.  Returns NULL on failure.
//	

static struct DirBuf *pdbMake(char *szDirName)
	{
	struct DirBuf *pdb = NULL;
	char szPath[_MAX_PATH];
	size_t cchdb;
	enum
		{
		enInternal, enNoDIR, enNoPath, enGood
		} enErr = enInternal;
	
	assert(NULL != szDirName);

	_cff2Bufs = max(_cff2Bufs, 1);
	cchdb = sizeof(struct DirBuf) +
		(sizeof(FILEFINDBUF2) * (_cff2Bufs - 1));
	if (NULL == (pdb = (struct DirBuf *) malloc(cchdb)))
		enErr = enNoDIR;
	else if (_makepath(szPath, "", szDirName, "*", "*"),
	         (NULL == (pdb->szPath = strdup(szPath))))
		enErr = enNoPath;
	else
		{
		pdb->bInvalid = 0;
		pdb->bRewind = 0;
		pdb->hdir = HDIR_CREATE;
		pdb->cff2 = _cff2Bufs;
		pdb->usSearchCount = 0;
		enErr = enGood;
		}
	switch (enErr)
		{
		case enNoPath:
			free((char *) pdb);
		default:
		case enNoDIR:
			pdb = NULL;
			break;
		case enGood:
			break;
		};
	return pdb;
	}
