struct dirent
	{
	char d_name[_MAX_PATH];
	};

typedef void DIR;

DIR *opendir(char *);
int closedir(DIR *);
struct dirent *readdir(DIR *);
void rewinddir(DIR *);
