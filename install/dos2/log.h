void FAR LogProgress(char *pszInfo);

#ifdef LOGGING
#define LOG(c)    LogProgress(c);
#else
#define LOG(c)    0
#endif
