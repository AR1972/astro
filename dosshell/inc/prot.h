;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

extern  VOID FARPUBLIC Shell_SetTmcText(TMC tmc, char *text);
extern  PENTRY AddFile(PTREE tree);
extern  int GetNextFile(char *path,PTREE tree,PENTRY rptr,int first, int ParentPathLength);
extern  int AddLateFile(char *path,struct fi_t far *rec,struct th_t far *tree,struct fi_t far *parent,int idx);
extern  void UpdateSiblings(struct th_t far *tree,struct fi_t far *parent,struct fi_t far *prev,struct fi_t far *rptr);
extern  void DelFileEntry(struct th_t far *tree,struct fi_t far *parent,struct fi_t far *node);
extern  int ChangeAttributes(struct th_t far *tree,struct fi_t far *node,char *placeholder,int count,int total,int verify);
extern  void MakeAttrLine(unsigned char x,unsigned char y,unsigned short isz,unsigned short bArg);
extern  unsigned short pascal ListProcAttrList(unsigned short tmm,char *sz,unsigned short isz,unsigned short tmc,unsigned short x,unsigned short y,unsigned short bArg);
extern  int pascal far FDlgattr(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  int pascal far FDlguser(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  void Append_Command(char far *str);
extern  void GetDialogParam(void );
extern  void UserDialog(void );
extern  void GetNextCmd(void );
extern  void ParseCmds(char far *commands);
extern  void DoCommand(char far *commands, char *startdir);
extern  unsigned char far MapToken2Color(int token,unsigned char def);
extern  void far DoSetColor(unsigned char set,int fore,int back);
extern void SelectColorGroup(int ColorSelection) ;
extern void ParseCommandLine(void);
extern  void far ColorBox(void );
extern  int pascal far FDlgcolor(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  void DoCopyFiles(void );
extern  void DoMoveFiles(void );
extern  void DoRenameFiles(void );
extern  void DoDelFiles(void );
extern  void DoViewFile(void );
extern  void DoChangeAttributes(void );
extern  void DoPrintFiles(void );
extern  void DoCreateDirectory(void );
extern  int GetDestPath(char *path);
extern  int pascal far FDlgCopyFiles(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  void DoFileOp(int type,char *path);
extern  void FormSelFileList(char *files,unsigned int buflen);
extern  int DisplayedFilesOk(void );
extern  int pascal far FDlgdelfiles(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  void com1(char *str);
extern  void com1i(int i);
extern  void FlopMsg(unsigned short dlm);
extern  void DumpBranch(struct fi_t far *walk,int indent);
extern  void DumpSel(void );
extern  int SelCountCheck(struct th_t far *tree);
extern  int CreateDirectory(PTREE tree,PENTRY parent,char *name, BOOL fRealCreation);
extern  int PromptAndCreateDir(struct th_t far *tree,struct fi_t far *parent);
extern  int pascal far FDlgcreatdir(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  int FindAndDelDirectory(void );
extern  int DelDirectory(struct fi_t far *deldir,struct th_t far *tree,int verify);
extern  void DisplayOptions(void);
extern  int pascal far FDlgdispopt(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  BOOL MarkAllTreeMatches(struct th_t far *tree,int doit);
extern  char *MakePatWellFormed(char *pat);
extern  int PatternsEquiv(char far *pat1,char far *pat2);
extern  unsigned short pascal ListProcCritList(unsigned short tmm,char *sz,unsigned short isz,unsigned short tmc,unsigned short x,unsigned short y,unsigned short bArg);
extern  int pascal far FDlgcrit(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  int CriticalDialog(void );
extern  BOOL FDelItemDialog(char * );
extern  unsigned int shell_findfirst(char *path,unsigned int attributes,struct find_t *buffer);
extern  void DoSingleTree(void );
extern  void DoDoubleTree(void );
extern  void DoFlatDisplay(void );
extern  void DoShareMode(void );
extern  int AreAllMatchedSelected(struct fi_t far *dir,struct th_t far *tree);
extern  void EnableDisableFileOps(void );
extern  unsigned int CountTreeSelMatFiles(struct th_t far *tree);
extern  unsigned int CountDirSelMatFiles(struct fi_t far *dir,struct th_t far *tree);
extern  unsigned short pascal ListProcErrList(unsigned short tmm,char *sz,unsigned short isz,unsigned short tmc,unsigned short x,unsigned short y,unsigned short bArg);
extern  int pascal far FDlgerr(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  int GetResponse(char *statusline,char *msg,int boxtype,unsigned int helpid);
extern  int DOSErrorBox(char *statusline,unsigned short err,unsigned int helpid);
extern  char *DOSErrorMsg(int errcode,int *box);
extern  void MessageBar(char *message, ISA isa,WORD force);
extern  void FileMgrStatusBar(struct th_t far *tree,struct fi_t far *node);
extern  void InitFileMgr(void );
extern  void InitIconCache(void );
extern  void SetUpScreen(void );
extern  void DoFileMgr(void );
extern  void ExitFileMgr(void );
extern  int FileMgrIdle(void );
extern BOOL StartProgramsIdle(void);
extern  void UpdateFileTree(int set);
extern  void DrawDrive(unsigned short list,unsigned short drive, ISA highlight);
extern  struct th_t far *HighlightDrive(unsigned short list,unsigned short drive, ISA highlight);
extern  BYTE GetSelectedDrive(unsigned short list);
extern  void SelectDrive(unsigned short list,unsigned short drive);
extern  void UpdateDrives(void );
extern  void DriveListKey(unsigned short list,unsigned short key, WORD state);
extern  int DriveMouse(unsigned short mx,unsigned short my,unsigned short msg);
extern  int DeleteFile(struct th_t far *tree,struct fi_t far *node,char *placeholder,int count,int total,int verify);
extern  int KillFile(char *statusline,char *path);
extern  int CopyFile(struct th_t far *tree,struct fi_t far *node,char *path,int count,int total,int verify);
extern  int CopyIt(struct th_t far *tree,struct fi_t far *node,char *dest,int count,int total);
extern  int MoveFile(struct th_t far *tree,struct fi_t far *node,char *path,int count,int total,int verify);
extern  int pascal far FDlgFileOpts(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  void DoFileOptions(void );
extern  void DoSelectAll(void );
extern  void DeselectTree(struct th_t far *tree);
extern  void DeselectDir(PENTRY, PTREE);
extern  void DoDeselectAll(void );
extern  int RenameFile(struct th_t far *tree,struct fi_t far *node,char *prognew,int count,int total,int ask);
extern  int PrintFile(struct th_t far *tree,struct fi_t far *node,char *placeholder,int count,int total,int verify);
extern  int pascal far FDlgRename(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  void DispFlatLeft(struct fi_t far *node,struct th_t far *tree,struct _wnd *pwnd,unsigned char XTopLeft,unsigned char YTopLeft,ISA isa);
extern  void CopyNameForTextOut(char *dest,char far *src);
extern  char *CopyNumberForTextOut(char *dest,unsigned long val, BOOL fFillBlanks);
extern  int GetDiskInfo(PTREE tree,char far *disklabel,unsigned long far *disksize,
											unsigned long far *diskavail) ;
extern  unsigned int GetTreeSelectedInfo(struct th_t far *tree,unsigned long far *psize);
extern  void PlotBmp(struct BITMAP *bmp,unsigned short x,unsigned short y,ISA color);
extern  void GetBmp(struct BITMAP *bmp,unsigned short x,unsigned short y);
extern  void AllocScreen(void );
extern  void ShellSaveScreen(void );
extern  void ShellRestoreScreen(void );
extern  void FrameCharRect(unsigned short top,unsigned short left,unsigned short bottom,unsigned short right, WORD linepat, ISA color);
extern  void FrameDialog(unsigned short top,unsigned short left,unsigned short bottom,unsigned short right);
extern  void brokenFrameButton(unsigned short top,unsigned short left,unsigned short bottom,unsigned short right);
extern  void NewFrameButton(unsigned short top,unsigned short left,unsigned short bottom,unsigned short right);
extern  void FrameButton(unsigned short top,unsigned short left,unsigned short bottom,unsigned short right);
extern  void FrameCharRectInset(unsigned short top,unsigned short left,unsigned short bottom,unsigned short right,unsigned short inx,unsigned short iny,ISA color);
extern  void EasyDrawBox(struct _wnd *pwd,unsigned char top,unsigned char left,unsigned char bottom,unsigned char right, ISA isa);
extern  void EasyDrawLine(unsigned char x1,unsigned char y1,unsigned char x2,unsigned char y2,int fdec);
extern  int RemoveQuotes(char *temp,char far *str);
extern  int AddQuotes(char *temp,char far *str);
extern  int BuildOutString(char *temp,char far *str);
extern  void InitMemory(void );
extern  unsigned short GetHandle(void );
extern  unsigned short pascal far HelpAlloc(unsigned short bytes);
extern  void pascal far HelpDeAlloc(unsigned short h);
extern  char far *pascal far HelpLock(unsigned short h);
extern  void pascal far HelpUnlock(unsigned short h);
extern  int pascal far OpenFileOnPath(char *fname,int fWrite);
extern  unsigned long pascal far ReadHelpFile(int fhandle,unsigned long fpos,char far *pData,unsigned short cbBytes);
extern  void pascal far CloseFile(int fhandle);
extern  int setupcontext(char far *constr);
extern  void HistPrev(void );
extern  void TopicPrev(void );
extern  void Next(void );
extern  void Index(void );
extern  void Contents(void );
extern  void DoNothing(void );
extern  void AccelerateInd(unsigned short code);
extern  int Tree2Path(PTREE tree,PENTRY node,char *str, int *plength);
extern  struct fi_t far *FindParent(struct fi_t far *node);
extern  int FindLastComponent(char *path);
extern  void catfname(char *dest,struct fi_t far *rcd);
extern  void FillRecName(char *path,struct fi_t far *rec);
extern  struct th_t far *FindTree(char *path, BYTE *driveind);
extern int FindNode(PTREE tree, char path[], PENTRY *theparent, PENTRY *thenode,
																	BOOL fForceFind) ;
extern  PENTRY File2Node(PTREE tree, PENTRY dir,char *file);
extern  int LoadCompactDir(PTREE tree, PENTRY new);
extern  int Internal2Normal(char far *dest,char far *src);
extern  void Normal2Internal(char far *dest,char far *src);
extern  void ScrunchFileName(char *dest,char *src, BOOL fSpecial) ;
extern  void FormCountStatusLine(char *statusline,char *szopstr,char *path,unsigned int count,unsigned int total, int indexofcnt);
extern  void Formxxofxx(char *str,int count,int total);
extern  void FormStringStatusLine(char *statusline,char *str1,char *str2, int maxlen);
extern  void far InitHistory(char *szcontext);
extern  void far AddToHistory(char *szcontext);
extern  void far InitCColor(void );
extern  void far InitGColor(void );
extern  int SetScreenMode(unsigned short mode);
extern  int far SelectScreenMode(struct _inst *inst);
extern  unsigned short far GetTheScreenMode(void );
extern  int InitializeScreen(void );
extern  void DoExit(void );
extern  int InitializeShell(void );
extern  int fstrncmp(char const far *nstr,char const far *fstr,int len);
extern  void fstrncpy(unsigned char far *d,unsigned char far *s,unsigned int len);
extern  void LaunchProgram(char far *programname,char far *parameters,int magiccookie);

extern  void far AddProgram(void);
extern  void far DelGroup(void);
extern  void far AddGroup(void);
extern  void far LaunchBox(void);
extern  void far DeleteProgram(void);
extern  void far StartAProgram(void);
extern  void far DoAssociateBox(void);
extern  void far ChangeGroup(void);
extern  void far ChangeProgram(void);
extern  void far HelpBox(void);

extern  unsigned long pascal far Pfnlaunch(unsigned short dlm,char *oof,unsigned short zong,unsigned short tmc,unsigned short wOld,unsigned short wParam);
extern  int pascal far FDlglaunch(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  unsigned short pascal far ListProcTreeList(unsigned short tmm,char *sz,unsigned short isz,unsigned short tmc,unsigned short x,unsigned short y,unsigned short bArg);
extern  unsigned short pascal far ListProcFileList(unsigned short tmm,char *sz,unsigned short isz,unsigned short tmc,unsigned short x,unsigned short y,unsigned short flags);
extern  int MakeTreeLine(struct _wnd *pwd,struct th_t far *tree,int line,unsigned char x,unsigned char y,int isfocus,int ischecked);
extern  int MakeFileLine(struct _wnd *pwnd,struct fi_t far *node,unsigned char x,unsigned char y,unsigned short flags,struct th_t far *tree);
extern  void DisplayNoFilesMsg(struct _wnd *pwd,unsigned short tmc,unsigned char xval,unsigned short y,unsigned short isz,unsigned short flags,struct th_t far *tree);
extern  PENTRY AllocTreeEntry(PTREE tree) ;
extern  PENTRY AllocTreeEntryCompactIfNeeded(PTREE tree) ;
extern  struct fi_t far *FindOpenSlot(struct th_t far *tree);
extern  unsigned int CountDirFiles(struct fi_t far *dir,struct th_t far *tree);
extern  unsigned int CountDirMatchedFiles(struct fi_t far *dir,struct th_t far *tree);
extern  void ResetTreeOptimizations(void );
extern  unsigned int CountTreeMatchedFiles(struct th_t far *tree);
extern  struct fi_t far *GetNthMatchedFile(struct fi_t far *dir,int count,struct th_t far *tree);
extern  struct fi_t far *GetNthFlatMatchedFile(struct th_t far *tree,int count);
extern  struct fi_t far *GetNthVisibleDir(struct th_t far *tree,int count);
extern  struct fi_t far *GetNthDir(struct th_t far *tree,int count);
extern unsigned GetDirInfo(PENTRY dir, PTREE tree, unsigned long *psize,
																unsigned int *pNumSelFiles) ;
extern  void JunkTree(PTREE tree);
extern  int CompactifyTree(PTREE tree,int sel);
extern  void ClobberFiles(PTREE tree);
extern  void ClobberDir(PTREE tree, PENTRY dir);
extern  void ClobberSel(PTREE tree, PENTRY parent, PENTRY curr);
extern  void PackDirs(PTREE tree);
extern  int CompactOne(int sel);
extern  void CompactAll(void );
extern  unsigned int GetIndexVisibleDir(struct fi_t far *dir,struct th_t far *tree);
extern  void SetSelectedDirFocus(void) ;

extern  unsigned int CountFilesToOperateOn(void );

extern  VOID ListBoxInit(ListBoxData *TestList,WORD (*ListProc)(),
	PWND pwd,WORD top,WORD left,WORD bottom,WORD right,char *title, 
	WORD tmc,WORD startfocusabsolute,WORD startfocusrelative);
extern  void CalcBarRect(struct ListBoxData *TestList,struct _rect *br);
extern  void CalcThumbRect(struct ListBoxData *TestList,struct _rect *tr);
extern  void DrawScrollBar(struct ListBoxData *TestList,int init);
extern  void DrawScrollBarArrow(struct ListBoxData *TestList,int UporDown);
extern  void UpdateScrollBar(struct ListBoxData *TestList,int value,int maxvalue);
extern BOOL PASCAL ScrollBarMouse(ListBoxData *TestList, WORD message);
extern  int GetNumItems(struct ListBoxData *TestList);
extern  void DoSetTitle(struct ListBoxData *TestList,char *title);
extern  void DoDrawTitle(struct ListBoxData *TestList);
extern  void SetTitle(struct ListBoxData *TestList,char *title);
extern  void FrameListBox(struct ListBoxData *TestList);
extern  void MakeListStupid(struct ListBoxData *TestList);
extern  void ListBoxHalt(struct ListBoxData *TestList);
extern  int DrawListItem(struct ListBoxData *TestList,unsigned short isz,int isfocus);
extern  void GlobalFocusBox(struct ListBoxData *TestList,int yesorno);
extern  void DoUpdateListBox(struct ListBoxData *TestList,unsigned short nlines);
extern  void QuickRedisplayList(struct ListBoxData *TestList);
extern  void DoRedisplayList(struct ListBoxData *TestList);
extern  void DoScrollListBox(struct ListBoxData *TestList,int amt,int updatenow);
extern  void FocusLineChange(struct ListBoxData *TestList,int amt);
extern  void PageDown(struct ListBoxData *TestList);
extern  void PageUp(struct ListBoxData *TestList);
extern  unsigned short Get_Focus_Line(struct ListBoxData *TestList);
extern  int ListMouse(struct ListBoxData *TestList,unsigned short x,unsigned short y,unsigned short message,int state);
extern  void InsertListItem(struct ListBoxData *TestList,unsigned short isz);
extern  void ListKey(struct ListBoxData *TestList,unsigned short key, unsigned short state);
extern  void UpdateListBox(struct ListBoxData *TestList);
extern  int ListBoxIdle(struct ListBoxData *TestList);



extern  int pascal far FDlglocate(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  void far FileLocateBox(void );
extern BOOL FAR Get_Date_and_Time_Strings(unsigned int theDate, unsigned int theTime,
	  char *datestr, char *timestr, BOOL force) ;
extern  void StatusBar(struct _wnd *Wind,int init);
extern  void UpdateMainTitleBar(char *szTitle);
extern  void PauseBeforeScreenErase(void );
extern  void far SetUpStartUpDirectory(void );
extern  void cdecl main(int argc,char * *argv);
extern  void pascal far Exit(int ex);
extern  void pascal far OutOfMemory(void );
extern  void *pascal far PbAllocWork(unsigned short cb);
extern  void pascal far FreeWork(void *pv);
extern  void far *pascal far LpbAllocWorkFar(unsigned short cb);
extern  void pascal far FreeWorkFar(void far *lpb);
extern  void far * far pascal AllocFarNormalized(WORD cb) ;
extern  void far PASCAL FreeFarNormalized(VOID FAR *lpb) ;
extern  void * *pascal far PpvAllocCb(unsigned short sb,unsigned short cb);
extern  void pascal far FreePpv(unsigned short sb,void * *ppv);
extern  void mstrncpy(unsigned char *d,unsigned char *s,unsigned int len);
extern  unsigned short pascal far cbSizePpv(unsigned short sb,void * *ppv);
extern  int pascal far Fcheckhandle(unsigned short sb,void * *ppv);
extern  void * *pascal far FReAllocPpv(unsigned short sb,unsigned short * *ppv,unsigned short size);
extern  void pascal far addchildhead(void );
extern  unsigned short far *pascal far LpwAllocDriverMem(unsigned short cw,unsigned short fmem);
extern  void pascal far FreeDriverMem(unsigned short far *lpw);
extern  struct _mtm *pascal far PmtmAddItem(struct _mnu * *hmnu,unsigned short idBefore,unsigned short idNew,unsigned char *sid,struct _mnu * *hmnuSub,unsigned short cwExtra,unsigned char bFlags);
extern  void initbar(struct _mnu * * *amenu);
extern  void easyitem(struct _mnu * * *amenu,char *name,int keyequiv,int (far *theproc)());
extern  void makefakemenu(struct _mnu * * *amenu,char *name,int keyequiv,int ishelp);
extern  void makeamenu(struct _mnu * * *amenu,char *name,int keyequiv);
extern  void easyseparator(struct _mnu * * *amenu);
extern  void easyenable(struct _mnu * * *amenu,int (far *theproc)(),int doenable);
extern  void setmenubar(struct _mnu * * *amenu,struct _wnd *towind);
extern  void MenuCommand(struct _wnd *pwnd,unsigned short mid);
extern  void FrameMenu(struct _wnd *pwnd,unsigned short mid);
extern  void FrameMenuBar(struct _wnd *pwnd);
extern  void DoInitMenus(void );
extern  void InitGlobalFocus(unsigned short focusstart);
extern  void NextGlobalFocus(void );
extern  void PrevGlobalFocus(void );
extern  void FileManagerKey(unsigned short key,unsigned short state);
extern  void MouseIdle(void );
extern  long pascal far WindProc(struct _wnd *pwnd,unsigned short message,unsigned short wParam,unsigned long LParam);
extern  void InitWindows(BOOL fFirstTime);
extern  void ScreenBox(void );
extern  void ShellSetCaption(char *sz);
extern  void DialogIsAlert(int set);
extern  unsigned long pascal far Pfndialog(struct _wnd *pwnd,unsigned short message,unsigned short wParam,unsigned long LParam);
extern  unsigned long pascal far Pfnpanel(struct _wnd *pwnd,unsigned short message,unsigned short wParam,unsigned long LParam);
extern  unsigned long pascal far Pfnbutton(struct _wnd *pwnd,unsigned short message,unsigned short wParam,unsigned long LParam);
extern  unsigned long pascal far Pfneditbox(struct _wnd *pwnd,unsigned short message,unsigned short wParam,unsigned long LParam);
extern  unsigned long pascal far Pfnfakeeditbox(struct _wnd *pwnd,unsigned short message,unsigned short wParam,unsigned long LParam);
extern  void DrawRadios(WORD, BOOL);
extern  unsigned long pascal far Pfnradiobutton(struct _wnd *pwnd,unsigned short message,unsigned short wParam,unsigned long LParam);
extern  unsigned long pascal far Pfnradiogroup(struct _wnd *pwnd,unsigned short message,unsigned short wParam,unsigned long LParam);
extern  void SetUpDialog(unsigned short tmcchild,char *title);
extern  void SetUpButtonForGraphics(unsigned short tmc);
extern void SetUpCheckBox(TMC tmc) ;
extern  void SetUpEditBox(unsigned short tmc, BOOL fRealOrNot, WORD maxcount, BOOL fHasInitialFocus);
extern  void SetUpRadioGroupForGraphics(unsigned short group, TMC prevtmc, TMC nexttmc);
extern  void SetUpRadiobuttonForGraphics(unsigned short tmc,int index);
extern  void SetUpPanelForGraphics(unsigned short tmc);
extern  void SetUpNothingButton(TMC tmc) ;
extern  WORD PrevScreenMode(INST *inst,WORD curmode) ;
extern  WORD NextScreenMode(INST *inst,WORD curmode) ;

extern  int pascal far FDlgscreen(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  void ShowInfo(void);
extern  int pascal far FDlgshowinfo(unsigned short dlm,unsigned short tmc,unsigned short wNew,unsigned short wOld,unsigned short wParam);
extern  void Updatesnext(struct th_t far *tree,struct fi_t far *parent,struct fi_t far *fil,
																	BOOL fDoSortedInsertion);
extern  void snextRootNotFirstUpdate(struct th_t far *tree,struct fi_t far *fil);
extern  void snextDirFirstUpdate(struct th_t far *tree,struct fi_t far *parent,struct fi_t far *fil);
extern  void snextDirNotFirstUpdate(struct th_t far *tree,struct fi_t far *parent,struct fi_t far *fil);
extern  struct fi_t far *GetLeftBrother(struct th_t far *tree,struct fi_t far *dir);
extern  struct fi_t far *PrevDFSDir(struct th_t far *tree,struct fi_t far *dir);
extern  struct fi_t far *PrevSibAndDFSDir(struct th_t far *tree,struct fi_t far *dir,struct fi_t far * *prev_dfs_dir);
extern  struct fi_t far *GetLastDirFile(struct fi_t far *dir);
extern  struct fi_t far *GetFirstDirFile(struct fi_t far *dir);
extern  struct fi_t far *GetLastRootFile(struct th_t far *tree);
extern  struct fi_t far *GetFirstRootFile(struct th_t far *tree);
extern  void InsertSystemOrder(struct th_t far *tree,struct fi_t far *fil);
extern  void InsertNotRootFirst(struct th_t far *tree,struct fi_t far *fil);
extern  void InsertNotDirFirst(struct th_t far *tree,struct fi_t far *parent,struct fi_t far *fil);
extern  void InsertAfter(struct fi_t far *fil,struct fi_t far *prev);
extern  void UpdateLatesnext(struct th_t far *tree,struct fi_t far *parent,struct fi_t far *fil,struct fi_t far *prev, 
																	BOOL fDoSortedInsertion);
extern  void HandleDeletesnext(PTREE tree, PENTRY parent, PENTRY node);
extern  void Marklastdir(PTREE tree);
extern  int HasNoSubDir(PTREE tree, PENTRY parent);
extern  void WalkDirsnext(struct th_t far *tree,struct fi_t far *dir);
extern  void FormDirectoryOrder(struct th_t far *tree,int doit);
extern void SetUpTreeForSort(PTREE tree) ;
extern  void SystemSort(PTREE tree, int doit);
extern void SortDirectory(PTREE tree, PENTRY dir) ;
extern  int date_cmp(struct fi_t far *a,struct fi_t far *b);
extern  int size_cmp(struct fi_t far *a,struct fi_t far *b);
extern  int disk_cmp(struct fi_t far *a,struct fi_t far *b);
extern  struct fi_t far *merge(struct fi_t far *a,struct fi_t far *b);
extern  struct fi_t far *isort(PENTRY head);
extern  struct fi_t far *mergesort(PENTRY c,unsigned short n);
extern  void strfcpy(char far *dst,char far *src);
extern  void strfncpy(char far *dst,char far *src,unsigned int cnt);
extern  int pmatch(char far *pat,char far *str,unsigned char mode);
extern  void get_comspec(char *buffer);
extern  int Printable(unsigned char c);
extern  unsigned int far AllocClosestTo1K(unsigned char far * *buffer,unsigned int lowend,unsigned int highend);
extern  int ViewFile(struct th_t far *tree,struct fi_t far *node,char *unused1,int unused2,int unused3,int unused4);
extern  void far WrapUpView(void );
extern  long pascal far ViewWindProc(struct _wnd *pwnd,unsigned short message,unsigned short wParam,unsigned long lParam);
extern  long VerifyPage(long line,int ascii);
extern  long SeekFwd(long bufpos,long lines);
extern  void DisplayPage(long bufpos,unsigned char lines,int ascii);
extern  void DisplayHeader(char *file);
int name_cmp(PENTRY entry1,PENTRY entry2);
int ext_cmp(PENTRY entry1,PENTRY entry2);
int quick_name_cmp(PENTRY entry1,PENTRY entry2);
int quick_ext_cmp(PENTRY entry1,PENTRY entry2);
extern BOOL translate_name(char far *src,char far *dest);
BOOL path_len_check(char *src, PENTRY dir, int len_diff) ;

extern void SetCriticalsToFail(void);
extern int  getdrivetype(int driveno);
extern int  Check_Spooler(void) ;
extern int  a_print_file(char far *name) ;
extern int FDoQuickCompare(void) ;
extern void SetCollatingTable(void) ;

extern unsigned char far * far cdecl GET_COMMAND_PTR(void);
extern unsigned char far * far cdecl GET_ARGS_PTR(void);
extern unsigned char far cdecl GET_WAIT_FLAG(void);
extern char far *realdma_ptr;
extern char far * cdecl GET_STARTUP_NAME(VOID);

extern  void List(int parent);
extern  int Assignment(void );
extern  void SectionBody(int section);
extern  void Section(void );
extern  void inifile(void );
extern  void parse(void );
extern  int Read_Ini_File(char *inifilename);
extern  unsigned int myfstrlen(unsigned char far *s);
extern  void indent(int fhandle,int level);
extern  void write_list(int fhandle,int token,int level);
extern  void cdecl main(int argc,char * *argv);
extern  int New_Symbol(char far *text,int length);
extern  int SubClassSymbol(int type);
extern  void Append_Symbol(int list,int new);

extern TOKEN Delete_Ith_Element(TOKEN list,int ith);

extern  int Get_Ith_Element(int list,int ith);
extern  int Get_List_Length(int list);
extern  int SetGet_KeyWord_Assignment(int section,int variable,int value,int set);
extern  int Get_KeyWord_Assignment(int section,int variable);
extern  int Set_KeyWord_Assignment(int section,int variable,int value);
extern  int Get_Identifier_Assignment(int section,char far *identifier);
extern  int Set_Identifier_Assignment(int section,char far *identifier,int value);
extern  void seektoendoflexeme(void );
extern  int lexeme_cmp(char far *src,int length,char far *lexeme);
extern  int String_To_Token(char far *string,int length);
extern  int lex(void );
extern  int match(int token);
extern  int SymbolAlloc(unsigned int size);
int name_cmp(PENTRY entry1,PENTRY entry2);
int ext_cmp(PENTRY entry1,PENTRY entry2);

extern void SetCriticalsToFail(void);
extern int  getdrivetype(int driveno);
extern int  Check_Spooler(void) ;
extern int  a_print_file(char far *name) ;

extern VOID FEnableMouseNest(BOOL onoroff) ;
extern VOID FInitMouseNest(void) ;

extern void InitializeStartPrograms(void);
extern int GetAttrResponse(void);
extern VOID PutUpBusyDialog(int ith);
extern VOID TakeDownBusyDialog(int ith);
extern VOID FAR DoSearchDisplay(BOOL fInitialize) ;
extern  int MakeSearchLine(struct _wnd *pwnd,struct fi_t far *node,unsigned char x,unsigned char y,unsigned short flags,struct th_t far *tree);
extern void HandleSearchQuit(void) ;
extern BOOL PromptAndCreateDir(PTREE tree, PENTRY parent) ;
extern void FormSelFileList(char *files, unsigned buflen) ;
extern unsigned CountTotalFilesSelected(void) ;
extern int DelDirectory(PENTRY deldir, PTREE tree, BOOL verify) ;
extern void FormPseudoPathName(char *path, PENTRY node, PTREE tree) ;
extern void EnableDisableForSearchMode(BOOL doenable1, BOOL doeanble2) ;
extern void EnableDisableTreeMenu(void) ;
extern void FlushMessages(void) ;
extern BOOL IsTreeBeingBuilt(void) ;
extern void CrippleFileMenuBar(void) ;

extern BOOL AreThereSubDirs(PTREE tree, PENTRY dir) ;
extern void ExpandDirVisibility(PTREE tree, PENTRY dir, BOOL fAllLevels) ;
extern void CollapseDirVisibility(PTREE tree, PENTRY dir) ;
extern unsigned char GetCollapseChar(PTREE tree, PENTRY dirnode) ;
extern void DrawDirIcon(PWND pwd, RX x, RY y, char ch, ISA isa) ;

extern void InsertDir(PTREE tree, PENTRY parent, PENTRY dir);

extern void DoExpand1Level(void);
extern void DoExpandBranch(void) ;
extern void DoExpandAll(void) ;
extern void DoCollapse(void) ;

extern BOOL initswapin(void) ;
extern BOOL initswapout(void) ;
extern VOID DoSwapIn(void) ;
extern VOID DoSwapOut(void) ;
extern VOID endswapin(void) ;
extern VOID endswapout(void) ;
extern VOID get_temp_dir(char *buffer,BOOL UseTemp) ;

// extern BYTE FAR * NEAR PASCAL LpbSaveGraphicArc(LPFN_LPB, WORD, AX, AY, AX, AY);
// extern VOID NEAR PASCAL RestoreGraphicArc(LPFN, AX, AY, AX, AY, BYTE FAR *);

/* The following commented out line was the prototype before CW Beta 5 */
// extern BYTE FAR * FAR PASCAL LpbSaveGraphicArc(LPFN_LPB, WORD, AX, AY, AX, AY);
extern BYTE FAR * FAR PASCAL LpbSaveGraphicArc(LPFN_LPB, AX, AY, AX, AY);
extern VOID FAR PASCAL RestoreGraphicArc(LPFN, AX, AY, AX, AY, BYTE FAR *);

extern BYTE FAR *MyLpbSaveGraphicArc(LPFN_LPB pfn, WORD cbSGA, AX left, AY top,
						  AX right, AY bottom) ;
extern VOID MyRestoreGraphicArc(LPFN pfn, AX left, AY top, AX right, AY bottom,
						  BYTE FAR *lpbSGA) ;
VOID PutUpStatusMessage(char *msg, int count) ;
BOOL TakeDownStatusMessage(int count, int total) ;

extern void DrawFocusMarker(PWND pwnd, RX rxText, RY ryText, RX rxGraphics,
				RY ryGraphics, WORD len, BOOL isfocus, BOOL issel, ISA isa) ;


BOOL AllocateHcabForOutOfMem(void) ;

extern void cdecl FormStringWithoutPlaceHolders(char far *dest, char far *src, ...) ;

extern int UnixChdir(char *path) ;

extern int cdecl chdir(char *path);
extern int cdecl mkdir (char *path) ;
extern int cdecl rmdir (char *path) ;


extern void HelpBox(void);
VOID FAR PASCAL Help(WORD hem, WORD hid,VOID *pv,WORD kk);
int PASCAL TextLineLen(char far *lpText, int nMaxLen, char cEndChar);

extern TMC  MyTmcDoDlg(VOID *pdlg, HCAB hcab) ;
extern VOID ShellMessageBox(char *messagetitle, char *message) ;
extern void Convert2AbsolutePath(char *dest, char *src) ;

extern void RepeatMove(char far *dest, char far *src, unsigned int cnt);

#ifdef DBCS
extern int  IsDBCSLeadByte(unsigned char);
extern int  CheckDBCSTailByte(unsigned char *,unsigned char *);
extern unsigned char    *DBCSstrupr(unsigned char *str);
extern unsigned char    *DBCSstrchr(unsigned char *str,unsigned char c);
#endif

