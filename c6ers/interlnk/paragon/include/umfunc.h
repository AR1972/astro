/***
* $Workfile:   umfunc.h  $
* $Revision:   1.13  $
*   $Author:   Dave Sewell  $
*     $Date:   11 Sep 1990  8:40:44  $
***/
/* File ..\absdisk.asm */
extern int cdecl absread(int drive, int sector, int numsectors, char *buff);
extern int cdecl abswrite(int drive, int sector, int numsectors, char *buff);
/* File ..\absdiskf.asm */
extern int cdecl absreadf(int drive, int sector, int numsectors, char far *buff);
extern int cdecl abswritef(int drive, int sector, int numsectors, char far *buff);
/* File ..\atwrite.asm */
extern int at_ascii_write(unsigned char *buff, int cnt_save);
/* File ..\b_keybrd.asm */
extern unsigned pascal paragon_bios_keybrd(unsigned service);
/* File ..\biostick.asm */
extern	unsigned _cdecl bios_ticks(void);
/* File ..\blockcmp.asm */
extern int cdecl blockcmp(void *memptr, unsigned int count, int value);
/* File ..\calcgmt.asm */
extern	long cdecl calcgmt(int days, int hours, int minutes, int seconds);
/* File ..\checksum.asm */
extern	unsigned short checksum(unsigned char *buff, int len);
/* File ..\crbuff.asm */
extern int cdecl crunch_buff(byte far *bp, unsigned cnt, byte far *dest, unsigned destsize);
/* File ..\desqview.asm */
extern void pascal dv_pause(void);
/* File ..\dosflush.asm */
extern  int pascal _dos_flush(int fd);
/* File ..\doslseek.asm */
extern  long pascal _dos_lseek(int fd, long offset, int method);
/* File ..\dskspace.asm */
extern	int dskspace(int drivenum, long *total, long *avail);
/* File ..\fixslash.asm */
extern void pascal fixslash(char *str);
/* File ..\flkforf.asm */
extern	char far * pascal flookforf(char far *str, int len, char far *buff, int bufflen);
/* File ..\fmemcmpf.asm */
extern int pascal fmemcmpf(byte far *p1, byte far *p2, unsigned int count);
/* File ..\fmemcpy.asm */
extern void pascal fmemcpy(void far *dest, const void *src, int count);
/* File ..\fmemcpyf.asm */
extern void pascal fmemcpyf(void far *dest, const void far *src, unsigned int count);
/* File ..\fmemsetw.asm */
extern void pascal fmemsetw(unsigned far *memptr, int value, unsigned int count);
/* File ..\fstrcpy.asm */
extern void pascal fstrcpy(void far *dest, const void *src);
/* File ..\fstrcpyf.asm */
extern void pascal fstrcpyf(void far *dest, const void far *src);
/* File ..\getwd.asm */
extern  void pascal getwd(char *buff);
/* File ..\htoi.asm */
extern int pascal htoi(char *pnt);
/* File ..\in_dst.asm */
extern  int cdecl in_dst(struct tm *);
/* File ..\inittick.asm */
extern	int pascal init_ticks(void);
/* File ..\kbcheck.asm */
extern  int kbcheck(void);
extern  int kbget(void);
/* File ..\lookfor.asm */
extern	char * pascal lookfor(char *str, int len, char *buff, int bufflen);
/* File ..\memcpyf.asm */
extern void pascal memcpyf(void *dest, const void far *src, int count);
/* File ..\memrchr.asm */
extern	char * cdecl memrchr(char *memptr, int value, int count);
/* File ..\memsetw.asm */
extern void pascal memsetw(short *memptr, short value, short count);
/* File ..\memspace.asm */
extern	void memspace(unsigned int psp, long *total, long *avail);
/* File ..\muldiv.asm */
extern unsigned pascal mul_div(unsigned op1, unsigned op2, unsigned divisor);
/* File ..\nullproc.asm */
/* File ..\patmatch.asm */
extern int _far _pascal patmatch(char *string, char *pattern);
/* File ..\pstrlen.asm */
extern unsigned int _fastcall pstrlen(char *str);
/* File ..\readtick.asm */
extern	unsigned int far pascal fread_ticks(void);
extern	unsigned int near read_ticks(void);
/* File ..\scrmem.asm */
extern void pascal restore_cursor(unsigned value);
extern void pascal locate(int row_col);
extern void pascal cursor_on(void);
extern void pascal cursor_off(void);
extern void pascal set_attribute(int attr);
extern void pascal dispmem(int pos, byte far *buff, int cnt);
extern void pascal clear(int ulpos, int lrpos);
extern void pascal scroll(int ulpos, int lrpos, int attr, int count, int direction);
extern void pascal fill(int ulpos, int height_width, int c);
extern void pascal shade(int ulpos, int height_width);
extern void pascal save_zone(int ulpos, int height_width, byte far *buff);
extern void pascal restore_zone(int ulpos, int height_width, byte far *buff);
extern void _far _pascal init_screen(void);
extern void _far _pascal restore_screen(void);
extern int _far _pascal init_scr(void);
extern void _far _pascal restore_scr(int restore_mode);
/* File ..\seldisk.asm */
extern  int pascal seldisk(char letter);
/* File ..\setcbrk.asm */
extern void cdecl setcbrk(void);
extern void cdecl resetcbrk(void);
/* File ..\setftime.asm */
extern void pascal setftime(int handle, unsigned long filedate);
/* File ..\setwd.asm */
extern  int pascal setwd(char *path);
/* File ..\strcpyf.asm */
extern void pascal strcpyf(void *dest, const void far *src);
/* File ..\string.asm */
extern int pascal fstrcmpf(char far *str1, char far *str2);
extern int pascal fstrcmp(char far *str1, char *str2);
extern int pascal fstrcmpif(char far *str1, char far *str2);
extern int pascal fstrnicmp(char far *str1, char *str2, unsigned int maxlen);
/* File ..\strlenf.asm */
extern unsigned int pascal strlenf(char far *str);
/* File ..\strlwrf.asm */
extern void pascal strlwrf(char far *string);
/* File ..\strrepc.asm */
extern  void pascal strrepc(char *str, int c, int repc);
/* File ..\strtcpy.asm */
extern void pascal strtcpy(char *dest, char *src, unsigned count);
/* File ..\strtcpyf.asm */
extern void pascal strtcpyf(char *dest, char far *src, unsigned count);
/* File ..\struprf.asm */
extern void pascal struprf(char far *string);
/* File ..\swapmem.asm */
extern	void pascal swapmem(char *p1, char *p2, int count);
/* File ..\ticks.asm */
extern	unsigned long pascal ticks(void);
/* File ..\uncrbuff.asm */
extern unsigned int pascal uncrunch_buff(byte far *bp, unsigned cnt, byte far *dest, unsigned destsize);
/* File ..\bioskybd.c */ 
extern  int pascal check_key(void );
extern  int pascal read_raw_key(void );
extern  int pascal grab_key(void );
extern  int pascal read_key(void );
extern  void _fastcall unread_key(int c);
extern  void pascal push_interrupt_keys(void );
extern  void pascal pop_interrupt_keys(void );
extern  void _fastcall enable_interrupt_key(int key,int (*proc)(void ));
extern  void _fastcall disable_interrupt_key(int key);
/* File ..\chwd.c */ 
extern  void pascal pushwd(void );
extern  char *pascal curwd(void );
extern  int pascal chwd(char *path);
extern  void pascal popwd(void );
extern  void pascal popallwd(void );
extern int _fastcall mkdirpath(char *dir_name);
extern int _fastcall force_chwd(char *path, int *created_dir);
/* File ..\dir.c */ 
extern  int pascal opendir(char *name,int dir_change,struct find_t *buffer);
extern  int pascal readdir(struct find_t *buffer);
/* File ..\..\dircat.c */ 
extern  void pascal dircat(char *dest,char *src1,char *src2);
/* File ..\..\dirpath.c */ 
extern  int pascal opendirpath(char *pname);
extern  int pascal readdirpath(struct path *path);
/* File ..\dosstat.c */ 
extern  int cdecl dosstat(char *fn,struct dosstat *st);
/* File ..\drawbox.c */ 
extern  void _fastcall draw_box(int ulpos,int height_width,int ltype,int attrib);
/* File ..\..\filetime.c */ 
extern  int pascal gettime(char *fname,long *time);
/* File ..\..\fixpath.c */ 
extern  void pascal fixpath(char *path,char *buff,int buffsize);
/* File ..\..\huffcnt.c */ 
extern  void huff_count(unsigned char *buff,int cnt);
/* File ..\..\huffdec.c */ 
extern  void huff_decode(unsigned char *src,int scnt,unsigned char *dest,int *dcnt);
/* File ..\..\huffenc.c */ 
extern  int huff_pad(unsigned char *dest);
extern  void huff_encode(unsigned char *src,int scnt,unsigned char *dest,int *dcnt);
/* File ..\..\huffman.c */ 
extern  void huff_init_count(void );
extern  void add_nodes(void );
extern  void huff_analyze(void );
extern  void huff_read_table(unsigned char *buff,int *cnt,long *packed_size);
extern  void huff_write_table(unsigned char *buff);
/* File ..\huntfor.c */ 
extern  long pascal huntfor(int fd,char *str,int len);
/* File ..\justify.c */ 
extern  void _fastcall justify_str(int pos,int len,int attrib,char *str,int justify);
/* File ..\..\lgmtime.c */ 
extern  int makewday(struct tm *date);
extern  long lgmtime(struct tm *date);
extern  long str_gmtime(char *string);
extern  long num_gmtime(int *num);
/* File ..\lzw2.c */ 
extern  int allocate_crunch_memory(int crunching,int big_crunch);
extern  void free_crunch_memory(void );
/* File ..\..\path.c */ 
extern  void pascal set_memory_error_proc(void (*err_proc)(char *));
extern  int pascal openpath(char *pname,int level);
extern  int pascal readpath(struct path *path,int level);
extern  void pascal closepath(int level);
/* File ..\..\pmatch.c */ 
extern  int pmatch(char *pattern,char *string);
extern  int wildword(char *s);
/* File ..\profiler.c */ 
extern  void start_profile(void );
extern  void stop_profile(void );
extern  void start_timer(int section_number,char *section_name);
extern  void stop_timer(int section_number);
/* File ..\..\regex.c */ 
extern  char *re_comp(char *sp);
extern  int cclass(char *set,char c,int af);
extern  int backref(int i,char *lp);
extern  int re_exec(char *p1);
/* File ..\rtc.c */ 
extern  int _rtc_getdate(struct dosdate_t *date);
extern  int _rtc_setdate(struct dosdate_t *date);
extern  int _rtc_gettime(struct dostime_t *time);
extern  int _rtc_settime(struct dostime_t *time);
/* File ..\run.c */ 
extern  int pascal run(char *cmd);
/* File ..\screen.c */ 
extern void _fastcall push_cursor(int on_flag, int location);
extern void pascal pop_cursor(void);
extern  void _fastcall push_attribute(int new_attribute);
extern  void pascal pop_attribute(void );
extern  void _fastcall fill_attr(int ulpos,int height_width,int c,int attrib);
extern  void _fastcall dispstr_attr(int pos,byte far *buff,int attr);
extern  void _fastcall dispmem_attr(int pos,byte far *buff,int cnt,int attrib);
extern  void _fastcall dispstr(int pos,byte far *buff);
extern  void _fastcall dispchar(int pos,int c);
/* File ..\..\strdiff.c */ 
extern  int strdiff(char *str1,char *str2,int ignore_case,int squeeze_white);
/* File ..\..\tsscanf.c */ 
extern  int cdecl tsscanf(char *str,char *format,...);
/* File ..\udscreen.c */ 
extern  void update_screen(unsigned char *screen);
/* File ..\..\watch.c */ 
extern  void pascal setwatch(void );
extern  long pascal watch(void );
extern  char *pascal seconds(long tenths);
extern int pascal fmemcmpw(unsigned far *p1, unsigned far *p2, unsigned int count);
extern int cdecl blockcmpf(void far *memptr, unsigned int count, int value);
extern int pascal wordcmpf(void far *memptr, unsigned count, unsigned value);
extern void pascal fwordcpyf(unsigned far *dest, const unsigned far *src, int count);
extern int pascal rmemcmp(byte *p1, byte *p2, unsigned int count);
extern int pascal frmemcmp(byte far *p1, byte far *p2, unsigned int count);
extern void _fastcall strsize(char *str, int *height, int *width);
extern unsigned long _fastcall mul32(unsigned op1, unsigned op2);
/* normaliz.c */
extern void huge *normalize(void huge *pnt);
