#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include "..\h\dirent.h"
#include "..\h\ftw.h"

struct odl
	{
	DIR *pd;
	struct odl *podlNext;
	char szFileCur[_MAX_FNAME + _MAX_EXT];
	};

static int ftwRecurse(char [], int (*)(char *, struct stat *, int), int, struct stat *, struct odl *);
static DIR *SearchDir(char [], char *);
static int CloseTop(struct odl *);
static int DoDir(char [], int (*)(char *, struct stat *, int), int, struct stat *, struct odl *);

int ftw(
char *path,
int (*fn)(char *, struct stat *, int),
int depth)
	{
	char szPath[_MAX_PATH];
	struct stat st;

	if ((NULL == path) || (NULL == fn) || (strlen(path) <= 0))
		{
		errno = EINVAL;
		return -1;
		}
	else
		{
		strcpy(szPath, path);
		return ftwRecurse(szPath, fn, max(depth, 1), &st, NULL);
		}
	}

static int ftwRecurse(
char szPath[_MAX_PATH],
int (*fn)(char *, struct stat *, int),
int depth,
struct stat *pst,
struct odl *podl)
	{
	struct odl odl;
	int rc = 0;

	assert((NULL != szPath) && (strlen(szPath) < _MAX_PATH));
	assert(depth >= 0);
	assert(NULL != pst);

	errno = 0;
	if (stat(szPath, pst))
		return (*fn)(szPath, pst, FTW_NS);
	else if ((pst->st_mode & S_IFDIR) != S_IFDIR)
		return (*fn)(szPath, pst, FTW_F);
	else if ((1 > depth) && (++depth, (rc = CloseTop(podl))))
		return rc;
	else if (NULL == (odl.pd = opendir(szPath)))
		return (*fn)(szPath, pst, FTW_DNR);
	else if (rc = (*fn)(szPath, pst, FTW_D))
		{
		closedir(odl.pd);
		return rc;
		}
	else
		{
		if ((NULL == podl) || (NULL == podl->pd))
			odl.podlNext = NULL;
		else
			odl.podlNext = podl;
		return DoDir(szPath, fn, depth, pst, &odl);
		}
	}

static int DoDir(
char szPath[_MAX_PATH],
int (*fn)(char *, struct stat *, int),
int depth,
struct stat *pst,
struct odl *podl)
	{
	int rc;
	struct dirent *pde;
	char *szFile = szPath + strlen(szPath);

	assert((NULL != szPath) && (strlen(szPath) < _MAX_PATH));
	assert(depth >= 0);
	assert(NULL != pst);
	assert(NULL != podl);
	
	if (('/' != *(szFile - 1)) && ('\\' != *(szFile - 1)))
		{
		*szFile = '\\';
		*++szFile = '\0';
		}
	for (rc = 0; (NULL != (pde = readdir(podl->pd))) && (0 == rc);)
		{
		if (!strcmp(pde->d_name, ".") ||
		    !strcmp(pde->d_name, ".."))
			continue;
		else
			strcpy(podl->szFileCur, strcpy(szFile, pde->d_name));

		if (rc = ftwRecurse(szPath, fn, depth - 1, pst, podl))
			;
		else if ((NULL == podl->pd) && ((*szFile = '\0'),
			 (NULL == (podl->pd =
				SearchDir(szPath, podl->szFileCur)))))
			rc = -1;
		else
			errno = 0;
		}
	if ((NULL != podl->pd) && closedir(podl->pd) && (0 == rc))
		rc = -1;
	return rc;
	}


static DIR *SearchDir(
char szPath[_MAX_PATH],
char *szFile)
	{
	register struct dirent *pde;
	register DIR *pd;

	assert((NULL != szPath) && (strlen(szPath) < _MAX_PATH));
	assert((NULL != szFile) && (strlen(szFile) > 0));

	errno = 0;
	if (NULL == (pd = opendir(szPath)))
		{
		return NULL;
		}
	while (NULL != (pde = readdir(pd)))
		if (!strcmp(pde->d_name, szFile))
			return pd;
	if (0 == errno)
		errno = ENOENT;	// Can't find file
	closedir(pd);		// Don't care if we fail, might change errno
	return NULL;
	}

static int CloseTop(register struct odl *podl)
	{
	register struct odl *podlPrev = podl;

	while (NULL != podl)
		{
		if (NULL == podl->podlNext)
			{
			int rc = closedir(podl->pd);
			podl->pd = NULL;
			podlPrev->podlNext = NULL;
			return rc;
			}
		else
			{
			podlPrev = podl;
			podl = podl->podlNext;
			}
		}
	return -1;
	}
