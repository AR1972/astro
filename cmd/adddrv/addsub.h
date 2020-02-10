;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

void	panic(int);
void	get_dosvar(void);
int	fm_strategy(int);
int	umblink(int);
int	dos_alloc(WORD, int);
void	dos_free(WORD);
int	dos_realloc(void);
int	dos_reallochi(void);
void	dos_maxalloc(void);
void	dos_maxallochi(void);
int	my_alloc(WORD, char, char*);
int	my_allochi(WORD, char, char*);
char	checkcom(LPSTR);
char	bufgetc(BUF *);
void	bufungetc(BUF *, char);
int	lpread(HANDLE, LPSTR, unsigned short);
int	checkkey(void);
void	Display_msg(int, int, int, int *, char);

