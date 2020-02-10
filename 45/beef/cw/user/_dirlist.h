/*
	COW : Character Oriented Windows

	_dirlist.h : dirlist call-backs for FBuildDirectoryList
*/


char * FAR PASCAL SzDirSpec(char *, char *, BOOL *);
VOID FAR PASCAL SetCurrentPath(char *);
VOID FAR PASCAL MakeDirName(char *, char *);
