        TITLE   uiinfhlp.asm - interface routines for new help engine.
;***
;uiinflp.asm
;
;       Copyright <C> 1985-1988, Microsoft Corporation
;
;Purpose:
;       Interface routines for the new help system.
;
;
;*******************************************************************************

        .xlist
        include version.inc
        .list
        UIINFHLP_ASM = ON


        include cw/version.inc
        include cw/windows.inc
        include cw/edityp.inc

        IncludeOnce architec
        Include     help.inc
        IncludeOnce qbimsgs
        IncludeOnce ui
        IncludeOnce uiint
        IncludeOnce uimenu
        IncludeOnce uinhelp

assumes DS,DATA
assumes ES,DATA
assumes SS,DATA

        subttl  DATA segment definitions.
        page

sBegin  DATA
        externB HelpFlags
        globalB fMessageBox,0,1         ; are we in a message box?
        externW efHelp                  ; Editmgr structure for Help Wnd.
        externW oCurTopic               ; offset to the current topic
        globalW iStaticHelpTopic,0,1    ; High word of NC during searches
        externB b$Buf1                  ; FILNAML-sized buffer
        externB b$Buf2
        externW iFileSize               ; Size of current help topic
        externW iCurRequestLine
        externW rgLineAttr              ; static attribute buffer
        externB fHelpAlloc

        HtSpot  EQU     b$Buf2          ; Static HotSpot structure

sEnd    DATA

        externFP GetEditMgrState
        externFP EditFilterWndProc
        externFP HelpSzContext
        externFP HelpHlNext
        externFP HelpNcPrev
        externFP HelpGetLineAttr
        externFP HelpGetLine
        externFP HelpNcNext


sBegin  UI
assumes CS,UI
        externNP toupper
        externNP GotoBookMark
        externNP SetBookMark
        externNP GetInitialPos
        externNP GetNextNc
        externNP SzSrchExcl
        externNP AppendBuf
        externNP CalcNc
        externNP MoveCursorPwnd
        externNP SizeHelpContext


;***
;DisplayHlpGeneric - Display either VarHelp or EngineHelp on screen
;
;Purpose:
;       Will deturmine the type of help specified and display it in
;       the help window.  It is expected to call this routine after
;       calling RetrieveHelpHistory, as its output parameters identically
;       match our input parameters.
;
;Entry:
;       DX:AX   - Help Topic (NC or Var Help ID)
;       CL      - Flags (EI_VARHELP only)
;                       0 => Engine help
;                      ~0 => Variable help
;
;Exit:
;       AL = 0    all ok
;       AL <> 0   failure (returns Error Code)
;****

cProc   DisplayHlpGeneric,<NEAR>
cBegin
        cCall   DisplayHlpWndNc         ; display the new topic

        ;AL = 0 if succeeded, error code otherwise

DisplayHlpGeneric_Exit:
cEnd


;***
;NextHotLink - Go to next hot link
;
;Purpose:
;       This routine will move the cursor to the next hot link in the
;       current topic.  Will wrap around the proper end of the help topic
;       to check for hot links if needed.
;
;Entry:
;       cLead : parameter to pass to HelpHlNext
;
;Exit:
;       None.
;
;****

cProc   NextHotLink,<NEAR>,<SI,DI>
parmW   cLead
cBegin
        ; This routine is only called when the help window is open
        ; and valid, thus we can assert HLP_GOTBUF and oCurTopic.

        DbAssertTst HelpFlags,ne,HLP_GOTBUF,UI,<NextHotLink:Buffers not initialized>


        DbChk   HoldBuf2                ; lock down the HotSpot

        ;Get current line#/column number

        mov     ax,EfHelp.EF_ipCur_ob   ; get the current column
        inc     ax                      ; make it 1 relative
        mov     HtSpot.colHS,ax         ; save it for HelpHlNext


        mov     ax,EfHelp.EF_ipCur_oln  ; current line #
        inc     ax                      ; make it 1 relative
        mov     HtSpot.lineHS,ax        ; save line number for help engine
        xor     di,di                   ; first time through

DbAssertRel     oCurTopic,ne,0,UI,<NextHotLink:oCurTopic invalid>

        mov     si,oCurTopic            ; get ptr to current topic
HotSpotRetry:
        lea     bx,[si].bdlHelpText.BDL_seg ; BX = handle of text seg
        xor     ax,ax                   ; ax = offset of text
        mov     cx,OFFSET DGROUP:HtSpot ; cx = offset of HotSPot
        push    HtSpot.lineHS           ; save row and column of hot spot
        push    HtSpot.colHS
        cCall   HelpHlNext,<cLead,BX,AX,DS,CX>
        pop     bx                      ; restore row and column of old hot spot
        pop     cx
        or      ax,ax                   ; did we get a hot linK?
        je      NextHotLink_Retry       ; no, go complain
        mov     ax,di                   ; save old loop counter
        inc     di                      ; one more time through loop
        or      ax,ax                   ; first time through?
        jne     MoveCursorToHotLink     ; no, it is a real hot link
        cmp     cx,HtSpot.lineHS        ; on the same row?
        jne     MoveCursorToHotLink     ; no, must be a real hot link
        cmp     bx,HtSpot.colHS         ; old column in current hot spot?
        jl      MoveCursorToHotLink     ; no, must be a real hot link
        mov     ax,HtSpot.ecolHS        ; AX = ending column of hot spot
        cmp     bx,ax                   ; old column in current hot spot
        ja      MoveCursorToHotLink     ; no, must be a real hot link
        inc     ax                      ; try again after the hot spot
        mov     HtSpot.colHS,ax         ; set position
        jmp     HotSpotRetry            ; and try again

NextHotLink_Beep:
        cCall   CowMoo                  ; beep the speaker
        jmp     short NextHotLink_Exit  ; and leave.

NextHotLink_Retry:
        cmp     di,1                    ; Have we already wrapped around?
        ja      NextHotLink_Beep        ; yes, nothing to find
        mov     di,2                    ; set wrapped flag
        mov     ax,1                    ; assume wrapping backwards
        mov     HtSpot.lineHS,ax        ; set line + col position
        mov     HtSpot.colHS,ax
        test    cLead,8000H             ; forward (+) or backwards (-)?
        jz      HotSpotRetry            ; we assumed right
        mov     ax,iFileSize            ; get last line number
        mov     HtSpot.lineHS,ax
        mov     HtSpot.colHS,-1         ; column position FFFF
        jmp     HotSpotRetry


MoveCursorToHotLink:
        ;Move the cursor to the position specified in the HotSpot structure
        mov     ax,HtSpot.lineHS        ; get the context relative row
        dec     ax                      ; make it 0 relative
        mov     cx,OFFSET DGROUP:wndHelp
        mov     bx,HtSpot.colHS         ; get the context relative column
        dec     bx                      ; make it 0 relative
        cCall   MoveCursorPwnd,<cx,ax,bx> ; move cursor to hotspot

NextHotLink_Exit:

        DbChk   FreeBuf2                ; release the HotSpot
cEnd


;***
;HelpBack - Backup one level of help (if possible)
;
;Purpose:
;       Implements ALT-F1.  The user asked us to go to the help topic
;       that was previously shown.
;
;
;       NOTE: SLIME: this routine uses iStaticHelpTopic for a temporary
;                    Dgroup storage location.  This should cause no problems,
;                    as HelpBack and searching are mutually exclusive.
;
;Entry:
;       None
;
;Exit:
;       None
;
;Uses:
;       Per Convention
;
;****
cProc   HelpBack,<PUBLIC, NEAR>,<SI,DI>
cBegin

;There are two cases to handle:
;       1) If the cursor is not at the start of a topic, move to start of topic
;          (note: the start of the topic is 0,0 and (if visible on the screen)
;          the first hot link.
;       2) If the cursor is at the start of a topic, move to previous topic
;
;General Algorithm when EI_VARHELP is turned on
;       Get Last topic from help history (should be displayed topic)
;       save topic
;       if (cursor not at start-of-topic)
;           Display Saved Topic  (will force cursor to start of topic)
;       else
;           while (New topic not displayed OK)
;               if (Topic Exists)
;                   Get Last topic from help history (topic to be displayed)
;                   Try to Display Topic
;               else
;                   Display Saved Topic

;
; We do not call HelpStart, as there is no reason to start the help system
; if it is not already started.  No Help => No Help Back => exit with error
;

        test    HelpFlags,HLP_GOTBUF    ; are we initialized?
        jne     Initialized             ; yes, see if we can get history
HelpBack_Err:
        cCall   CowMoo                  ; beep the speaker
DJMP    jmp     short HelpBack_Exit     ; and return

Initialized:
        cCall   RetrieveHelpHistory     ; get the currently displayed item
        jcxz    HelpBack_Err            ; no current topic? exit
                                        ; (possible if no help ask for yet)
        mov     si,dx                   ; set SI:DI = context info.
        mov     di,ax



        ;Test for cursor at begining of topic

        xor     ax,ax                   ; 0 to compare against
        cmp     fHelpVisible,al         ; help window visible?
        je      CurrentTopic            ; no, goto current topic

        cmp     EfHelp.EF_ipCur_ob,ax   ; at column 0?
        jne     CheckHotLink            ; no, goto initial position
        cmp     EfHelp.EF_ipCur_oln,ax  ; on first line?
        je      PreviousTopic           ; yes, goto previous topic

CheckHotLink:
        cCall   GetInitialPos           ; (DX,AX) is initial cursor position
        cmp     EfHelp.EF_ipCur_ob,ax   ; in the right column?
        jne     CurrentTopic            ; no, redisplay current topic
        cmp     EfHelp.EF_ipCur_oln,dx  ; in the right row?
        jne     CurrentTopic            ; no, redisplay current topic

PreviousTopic:
        cCall   RetrieveHelpHistory     ; get the item to goto
        jcxz    CurrentTopicBeep        ; none, beep and exit (CX = 0)

        cCall   DisplayHlpWndNc         ; display the topic
        cmp     al,HELP_HANDLED         ; did we get an error?
        je      CurrentTopic            ; yes, try to clean up and exit
        or      al,al                   ; did we succeed?
        jne     PreviousTopic           ; no, try topic before it

        jmp     short HelpBack_Exit     ; we succeeded, exit


CurrentTopicBeep:
        cCall   CowMoo                  ; tell user of error
CurrentTopic:
        mov     dx,si
        mov     ax,di
        cCall   DisplayHlpWndNc         ; display the topic

        ;any error at this point must have been caused by OOM or Missing varhelp

        or      al,al                   ; could we redisplay?
        je      HelpBack_Exit           ; yes, we are done
        cCall   GiveHelpOOM             ; display OOM message

HelpBack_Exit:
        DbAssertRel     curHelpFile,e,0,UI,<HelpBack: help file open>

cEnd

;***
;CmdHelpPrev - Goto Previous Help Topic
;
;Purpose:
;       The user has requested that we goto the previous help topic. Beeps
;       if this is not possible or the help window is not open.
;
;Entry:
;       None.
;
;Exit:
;       None.
;
;Uses:
;       Per C Convention
;
;****

cProc   CmdHelpPrev,<PUBLIC,NEAR>
cBegin
        cmp     fHelpVisible,0                  ; help window visible?
DJMP    je      CmdHelpBeep                     ; no, beep and exit

DbAssertTst     HelpFlags,ne,HLP_GOTBUF,UI,<CmdHelpPrev:Help window open but not HLP_GOTBUF>
        and     HelpFlags,NOT (HLP_FAILFNF OR HLP_FAILOOM) ;  clear flags
        mov     bx,oCurTopic                    ; get ptr to current topic
        push    Word Ptr [bx].ContextNum+2      ; parameter to HelpNcPrev
        push    Word Ptr [bx].ContextNum
        cCall   HelpNcPrev                      ; get next NC
        mov     cx,ax                           ; is it 0? (non available)
        or      cx,dx
        jne     CmdHelpDisplay                  ; no, try to display it
        test    HelpFlags,HLP_FAILFNF OR HLP_FAILOOM ; already displayed msg
        jz      CmdHelpBeep                     ; no, topic not found=>beep
cEnd

;***
;CmdHelpNext - Goto Next Help Topic
;
;Purpose:
;       The user has requested that we goto the next help topic. Beeps
;       if this is not possible or the help window is not open.
;
;Entry:
;       None.
;
;Exit:
;       None.
;
;Uses:
;       Per C Convention
;
;****

cProc   CmdHelpNext,<PUBLIC,NEAR>
cBegin

        cmp     fHelpVisible,0                  ; help window visible?
        je      CmdHelpBeep                     ; no, beep and exit

DbAssertTst     HelpFlags,ne,HLP_GOTBUF,UI,<CmdHelpNext:Help window open but not HLP_GOTBUF>
        mov     bx,oCurTopic
        mov     dx,Word Ptr [bx].ContextNum + 2 ; DX:AX = old context num
        mov     ax,Word Ptr [bx].ContextNum
        cCall   GetNextNc                       ; Set DX:AX
        jz      CmdHelpError                    ; check for errors
CmdHelpDisplay:                                 ; Called from CmdHelpPrev
        cCall   DisplayHlpWndNc
        or      al,al                           ; did we succeed?
        je      CmdHelpNext_Exit                ; yes, return.
CmdHelpError:
        cmp     al,HELP_HANDLED                 ; Will someone else handle?
        je      CmdHelpNext_Exit                ; yes, just exit
DbAssertRelB    al,e,HELP_NF,UI,<CmdHelpNext:Illegal error>

CmdHelpBeep:                                    ; NOTE: shared label
        cCall   CowMoo                          ; beep the speaker
CmdHelpNext_Exit:
cEnd

;Routine added with [1]
;***
;NormalizeTopic - Convert a context number to a standard form
;
;Purpose:
;       The current implementation of the help engine allows a piece of text
;       to have multiple context strings, thus multiple context numbers.
;       While searching, we use the context number to specify when we have
;       wrapped around the file and reached the starting point.  Thus, we
;       have to Normalize the topic number so it is the same one that will
;       be returned from HelpNcPrev or HelpNcNext.
;
;Entry:
;       AX : topic ID
;
;Exit:
;       if (CX != 0)
;          DX:AX : topic number
;       else
;          error (will have been signaled)
;
;Uses:
;       Per C Convention
;****

cProc   NormalizeTopic,<NEAR>
cBegin
        push    ax                      ; preserve Topic ID
        mov     dx,iStaticHelpTopic     ; DX:SI = context number
        ccall   HelpNcNext,<iStaticHelpTopic,AX> ; get previous one
        pop     bx                      ; BX = topic ID
        mov     cx,ax                   ; did we succeed?
        or      cx,dx
        jz      PrevNext                ; no, try to get one after the topic ID
        cCall   HelpNcPrev,<DX,AX>      ; yes, do a Prev to get normalized NC
        jmp     short CheckReturn       ; and return it
PrevNext:
        cCall   HelpNcPrev,<iStaticHelpTopic,bx> ; get NC before topic ID
        mov     cx,ax                   ; did this succeed?
        or      cx,dx
        jz      NormalizeExit_Err       ; no, exit with error
        cCall   HelpNcNext,<DX,AX>      ; get topic after that one

CheckReturn:
        mov     cx,ax                   ; set error condition
        or      cx,dx

NormalizeExit_Err:
cEnd


;***
;ChngHelpCurTopic - Cleans up after a help search
;
;Purpose:
;       The three routine set (GetHelpCurTopic, GetHelpNextTopic,
;       ChngHelpCurTopic) are used for a search or scan of the help file.
;
;       This routine will take a topic ID that was used to terminate
;       the search and prepare things for continued operation of the
;       help system.  This routine should only be called if we are
;       exiting from a search in a topic other than the original
;       topic, and want to change that topic to be the new current topic.
;
;Entry:
;       CX = previous topic
;
;Exit:
;       None.
;****
cProc   ChngHelpCurTopic,<NEAR>
cBegin

DbAssertTst     HelpFlags,e,HLP_VARHELP,UI,<ChngHelpCurTopic:Called when HLP_VARHELP>
        mov     ax,cx                   ; Set DX:AX to NC of topic
        mov     dx,iStaticHelpTopic
        or      dx,dx                   ; check to see if it is valid
        je      ChngHelp_Exit           ; no, just exit (we will close help wnd)

        inc     cx                      ; Topic ID = UNDEFINED ?
        je      ChngHelp_Exit           ; brif - identifier is invalid.

DbAssertTst     HelpFlags,ne,HLP_GOTBUF,UI,<ChngHelpCurTopic:iStaticHelpTopic non 0 without HLP_GOTBUF>

        cCall   RecordHelpHistory       ; record topic in help history list
ChngHelp_Exit:
        cCall   DrawDebugScr            ; make sure screen is redrawn so
                                        ; we get proper help title.
cEnd

;***
;GetHelpNextTopic - Prepares the next topic for looking
;
;Purpose:
;       The three routine set (GetHelpCurTopic, GetHelpNextTopic,
;       ChngHelpCurTopic) are used for a search or scan of the help file.
;
;       This routine will take a Topic ID and return the next physical
;       Topic ID.  If the presented Topic ID was the last one in a section,
;       this routine will wrap around to the begining of the section.
;
;
;Entry:
;       CX = previous topic
;
;Exit:
;       AX = new Topic Identifier if engine help
;            0                    if var help (will work as topic id)
;            UNDEFINED            if error
;****

cProc   GetHelpNextTopic,<NEAR>,<DI>
cBegin
        DbChk   HoldBuf1                ; use BUF1 as a temporary buffer

        test    HelpFlags,HLP_COMPRESS  ; help shutting down
DJMP    jnz     NextTopic_Exit          ; yes, exit


        mov     dx,iStaticHelpTopic     ; get static high word of NC
        or      dx,dx                   ; is it 0 (help system shut down)
DJMP    je      NextTopic_Fail          ; yes, return error

DbAssertTst     HelpFlags,ne,HLP_GOTBUF,UI,<GetHelpNextTopic:iStaticHelpTopic non 0 without HLP_GOTBUF>

        mov     ax,cx                   ; DX:AX = old topic number
        cCall   GetNextNc               ; DX:AX = next context number
DJMP    jnz     GotNextTopic            ; we were successful

        cmp     al,HELP_HANDLED         ; error message already displayed?
DJMP    je      NextTopic_Fail          ; yes, return error code

DbAssertRelB    al,e,HELP_NF,UI,<GetHelpNextTopic:illegal error from GetNextNc>



        ;Goto the first topic of this section of the file.  We are guarenteed
        ;that the topic h.pg1 (MSG_FirstContextStr) exists at the begining of
        ;each section of the help system.  Thus, we pick off the file name
        ;part of any context string of the section and goto filename!h.pg1


        mov     bx,oCurTopic                    ; get ptr to a legal buffer
        mov     di,OFFSET DGROUP:B$Buf1         ; store results in B$Buf1
        push    ds                              ; far ptr to buffer
        push    di
        push    Word Ptr [bx].ContextNum+2      ; context number
        push    Word Ptr [bx].ContextNum
        cCall   HelpSzContext                   ; get current filename
        or      ax,ax                           ; was the call successful?
        je      NextTopic_Fail                  ; no, exit with error

        cCall   szSrchExcl,<DI>                 ; AX = pos of '!' or 0
DbAssertRel     ax,ne,0,UI,<szComposeContext:Illegal string from HelpSzContext>

        mov     bx,MSG_FirstContextStr          ; message to append
        cCall   AppendBuf,<AX,BX>               ; stick it on
        xchg    ax,bx
        mov     Byte Ptr [bx],0                 ; and 0 terminate it.

        ;Get the context number + context for this name
        ;param for CalcNc pushed above

        cCall   CalcNc,<DI>             ; convert to context number
        jcxz    NextTopic_DbFail        ; brif we couldn't locate it

        cCall   NormalizeTopic          ; normalize topic number
        jcxz    NextTopic_Fail          ; brif it can't be normalized

GotNextTopic:
        mov     di,ax                   ; save value to be returned
        cCall   BufferNc                ; BX = ptr to buffered context
        or      al,al                   ; error in buffering?
        jne     NextTopic_Fail          ; yes, return with error code
        lea     ax,[bx].bdlHelpText     ; get ptr to BDL
        cCall   SizeHelpContext,<ax>    ; AX = # lines in this topic
                                        ; sets iFileSize
        xchg    ax,di                   ; restore return value
        jmp     short NextTopic_Exit    ; and return

NextTopic_DbFail:
        DbAssertRelB al,ne,HELP_NF,UI,<GethelpNextTopic:h.pg1 missing from file>


        ;If we get a fatal error, we have to close the help window.  There
        ;is no guarentee that we can redisplay the original topic, and to
        ;display some random topic would be poor.  Thus, make sure the
        ;bit to close the help window in DoDrawDebugScr is set.

NextTopic_Fail:
        or      HelpFlags,HLP_COMPRESS  ; close help window at earliest time
        mov     ax,UNDEFINED            ; assume we will get an error
NextTopic_Exit:
        DbChk   FreeBuf1                ; release buffer
cEnd

;***
;GetHelpCurTopic - Start search and returns the current topic id
;
;Purpose:
;       The three routine set (GetHelpCurTopic, GetHelpNextTopic,
;       ChngHelpCurTopic) are used for a search or scan of the help file.
;
;       This routine will prepare for a linear scan of the help file
;       and will return the Topic Id for the currently displayed help
;       topic.
;
;       A Topic ID is a word that references a help topic.  It is the
;       lower word of a NC, so it is valid only as long as we do not
;       shut down the help system.
;
;Entry:
;       None.
;
;Exit:
;       AX = cur Topic Identifier if engine help
;            0                    if var help (will work as topic id)
;            UNDEFINED            if error
;****

cProc   GetHelpCurTopic,<NEAR>
cBegin
        test    HelpFlags,HLP_GOTBUF    ; are we initialized?
        mov     ax,UNDEFINED            ; assume not
        je      CurTopic_Exit           ; brif not - give error and ret.


DbAssertRel     oCurTopic,ne,0,UI,<GetHelpCurTopic:oCurTopic invalid>
        mov     bx,oCurTopic            ; get a pointer to the current topic
        mov     ax,Word Ptr [bx].ContextNum ; get topic specific part of NC
        mov     dx,Word Ptr [bx].ContextNum+2
        mov     iStaticHelpTopic,dx     ; save static part of NC
        cCall   NormalizeTopic          ; Normalize the topic number
        inc     cx                      ; JCXNZ
        loop    CurTopic_Exit           ; brif success
        mov     ax,-1                   ; set failure flag
CurTopic_Exit:

cEnd

;Added with [4]
;***
;RestoreHelpTopic - Return help system to state before a search
;
;Purpose:
;       The search code needs the original help topic restored to the screen.
;       We get the topic from the help history list and redisplay it.
;
;       Note: we can not use DisplayHlpWndNc, as it will change the position
;       of the cursor and resize the window.
;
;
;Entry:
;       None.
;
;Exit:
;       None.
;
;****

DbPub   RestoreHelpTopic
cProc   RestoreHelpTopic,<NEAR>
cBegin
        test    HelpFlags,HLP_VARHELP   ; are we in variable help?
        jnz     exit                    ; yes, exit. nothing to restore
        cCall   RetrieveHelpHistory     ; get the last history item
DbAssertRel     cx,ne,0,UI,<RestoreHelpTopic:History list empty>
        push    ax                      ; save context number
        push    dx
        cCall   BufferNc                ; load context into memory, al=err code
        pop     dx                      ; restore context number
        pop     cx
        or      al,al                   ; any errors?
        jne     RestoreTopic_Fail       ; yes, handle them
        push    bx                      ; save ptr to help context
        xchg    ax,cx                   ; DX:AX = context number
        cCall   RecordHelpHistory       ; save this in the history list
        pop     bx                      ; return ptr to help context
        lea     ax,[bx].bdlHelpText     ; Get ptr to BDL of this help topic
        cCall   SizeHelpContext,<ax>    ; calculate size, set iFileSize
        jmp     short exit              ; get out of here

RestoreTopic_Fail:
        DbAssertRelB    al,e,HELP_HANDLED,UI,<RestoreHelpTopic:Unable to regenerate initial topic>
        or      HelpFlags,HLP_COMPRESS  ; no error should have happened, we
                                        ; have no idea what state we are in,
                                        ; so close help window
exit:
cEnd

;***
;GetHelpTitle - Returns the title for the currently displayed help item
;
;Purpose:
;       Calculates the title that should go on the help window and
;       returns it in bufStdMsg.
;
;Entry:
;       CX = max title length allowed
;
;Exit:
;       AX = number of characters in title
;       bufStdMsg = contains title.
;
;Uses:
;       Per Convention
;****

DbPub   GetHelpTitle
cProc   GetHelpTitle,<NEAR>,<SI>
cBegin
        mov     si,cx                   ; SI = cbMaxTitle
        mov     ax,MSG_HelpTitleQH      ; first part of title ("MS-DOS Help: ")
        test    cmdSwitches,CMD_SW_QHELP ; /QHELP viewer?
        jnz     ght1                    ;   YES, got title

        mov     ax,MSG_HelpTitle        ; Yes normal help window title

ght1:
        cCall   ListStdMsg,<AX>         ; put in bufStdMsg

        mov     bl,HelpFlags                    ; get a copy of the flags
        and     bl,HLP_GOTBUF or HLP_COMPRESS   ; mask out all but these
        cmp     bl,HLP_GOTBUF                   ; brif NOT HLP_GOTBUF or
        jne     GetHelpTitle_Exit               ;  HLP_COMPRESS

; HLP_INHELP will only be set if we are about to do something that can
; put up a dialog/message box.  If this routine is called with it set,
; then we are trying to redraw the screen after the box has been removed.
; This can be dangerious, as we may not be in a state where we can
; generate a help title.  So return with what we have.

        test    HelpFlags,HLP_INHELP    ; are we called recursively?
        jz      NotRecursiveGHT         ; no, try to get a title
        cmp     fMessageBox,0           ; are we in a dialog box?
        jnz     GetHelpTitle_Exit       ; yes, exit with what we have.

NotRecursiveGHT:


HelpTitleNotVarHelp:
        mov     bx,oCurTopic
        or      bx,bx                   ; current topic invalid?
        je      GethelpTitle_Exit       ; yes, return with what we have
        lea     ax,[bx].bdlHelpText     ; get ptr to the BDL

        cCall   GetHelpTitleStr,<ax,si> ; AX = total length of title


GetHelpTitle_Exit:
        DbAssertRel ax,be,si,UI,<GetHelpTitle: title too big>
cEnd

;***
;GetHelpLine - Get a line of help text
;
;Purpose:
;       This routine is called by the edit manager to display a line
;       of text on the screen.  This routine must take care of all
;       the cases associated with ressyncing the virtual line mechanism,
;       as well as detecting when a ressync would be needed.
;
;       Since this routine is called from COW, the line numbers are 0 relative.
;       The help system is 1 relative (because of the HelpEngine) so we
;       do the conversions on the fly.
;
;Entry:
;       CX = iLineNum - virtual line number of line to retrieve
;       DX = szBufPtr - pointer to buffer in which to store line
;       AX = cbMaxSize - maximum size of buffer
;
;Exit:
;       AX = # bytes in line
;
;****

cProc   GetHelpLine,<NEAR>,<SI,DI>
cBegin
        mov     si,ax                           ; save entry params
        mov     di,dx
        mov     al,HelpFlags                    ; get a copy of the flags
        and     al,HLP_GOTBUF or HLP_COMPRESS   ; mask out all but these
        cmp     al,HLP_GOTBUF                   ; brif HLP_GOTBUF and
        je      CheckHelpOwner                  ;  NOT HLP_COMPRESS

GetLineError:
        xor     ax,ax                           ; number of characters
GetLine_NoAttr:

        ; signal GetHelpLineAttr that it can't lookup attributes

        mov     iCurRequestLine,UNDEFINED
        jmp     short GetHelpLine_Exit          ; and return

CheckHelpOwner:

        inc     cx                              ; make 1 relative



GotHelpLine:

        mov     bx, oCurTopic                   ; get current topic pointer
        or      bx,bx                           ; is it 0?
        je      GetLineError                    ; yes, no current topic set

        mov     iCurRequestLine,cx              ; set line # offset for attribs

        lea     bx,[bx].bdlHelpText.BDL_Seg          ; BX = handle text seg
        xor     ax,ax                                ; AX = text offset
        cCall   HelpGetLine,<CX,SI,DS,DI,BX,AX>      ; put text into buff.
        or      ax,ax                                ; did we get the line?
        je      GetLineError                         ; no, signal error

        ;AX = number of characters in buffer = return value

GetHelpLine_Exit:
cEnd


;***
;GetHelpLineAttr - return attributes for previously fetched line
;
;Purpose:
;       This routine will return the line attributes for the
;       line that was retrieved with the last call to GetHelpLine.
;
;       Note: No call that could change the state of the help system
;             should be called between GetHelpLine and this routine.
;
;Entry:
;       none.
;
;Exit:
;       AX = near ptr to string containing attributes
;
;Uses:
;       Per Convention
;
;****
cProc   GetHelpLineAttr,<NEAR>
cBegin
        .errnz (-1) - (UNDEFINED)

        mov     ax,iCurRequestLine              ; did GetHelpLine return bogus
        inc     ax                              ; line (== -1 = UNDEFINED)?
        jne     Get_Attr                        ; no, line is buffered

DefaultAttrs:
        mov     bx,OFFSET DGROUP:rgLineAttr     ; pointer to static buffer
        push    bx                              ; save for return ptr
        mov     ax,UNDEFINED
        mov     [bx + LA_cb],ax
        mov     [bx + LA_attr],isaSyntaxHelp
        mov     [bx + (size LINEATTR) + LA_attr],ax

DJMP    jmp     short GetAttr_End

Get_Attr:

; we assume that nothing in the help system will change from the call to
; HelpGetLine.  Thus, we do not have to check that help is up and running,
; as HelpGetLine will zero iCurRequestLine if there are ANY problems.
; Also, CompressHelp will zero iCurRequestLine.

DbAssertTst     HelpFlags,ne,HLP_GOTBUF,UI,<GetHelpLineAttr: HLP_GOTBUF not set>
DbAssertRel     oCurTopic,ne,0,UI,<GetHelpLineAttr:Invalid oCurTopic>

        mov     bx,oCurTopic
        lea     bx,[bx].bdlHelpText.BDL_seg     ; get handle of topic seg

; a-emoryh Added more space for Dos6 online help index strings
        mov     ax,CB_bufStdMsg + 6 + 16 + 70   ; maximum size of buffer

        mov     cx,OFFSET DGROUP:bufStdMsg      ; buffer to store results in
        push    cx                              ; save for mapping start
        xor     dx,dx                           ; offset into topic segment
        cCall   HelpGetLineAttr,<iCurRequestLine,AX,DS,CX,BX,DX>
        pop     bx                              ; pointer to buffer
        or      ax,ax                           ; did we succeed?
        jz      DefaultAttrs                    ; no, use default attrs

        ;Map attributes from HELP format to COW format

        push    bx              ; save ptr to buffer for return

AttrList:
        mov     ax,[bx].LA_attr ; get first attribute
        inc     ax              ; is it -1 (end of list)
        je      GetAttr_End     ; yes, we are done
        dec     ax              ; restore attribute
        DbAssertTst ax,e,<NOT (A_BOLD OR A_UNDERLINE OR A_ITALICS)>,UI,<GetHelpLineAttr: Illegal attributes set>
        mov     cx,isaBold      ; assume bold attribute
        test    ax,A_BOLD       ; does it have any bold in it?
        jne     GotAttr         ; yes, update and loop
        mov     cx,isaUnderline ; assume underline attribute
        test    ax,A_UNDERLINE  ; does it have any underline in it?
        jne     GotAttr         ; yes, update and loop
        mov     cx,isaItalic    ; assume italics attribute
        test    ax,A_ITALICS    ; does it have any italics in it?
        jne     GotAttr         ; yes, update and loop
        mov     cx,isaSyntaxHelp; it must be default

GotAttr:
        mov     [bx].LA_attr,cx ; update attribute
        add     bx,Size LINEATTR; point to next attribute/size pair
        jmp     short AttrList  ; go modify this one.

GetAttr_End:
        pop     ax              ; retrieve pointer to begining of buffer

cEnd

;***
;GetHelpFileSize - Returns the number of lines in the help file
;
;Purpose:
;       This routine is used to set the scroll bar, and to establish
;       the maximum line number in the file.  Since we currently use
;       a virtual file, we will return a virtual size.
;
;Entry:
;       None.
;
;Exit:
;       AX = size of file in lines
;
;Uses:
;       AX
;
;****
DbPub   GetHelpFileSize
cProc   GetHelpFileSize,<NEAR>
cBegin

; HLP_INHELP will only be set if we are about to do something that can
; put up a dialog/message box.  If this routine is called with it set,
; then we are trying to redraw the screen after the box has been removed.
; This can be dangerious, as we may not be in a state where we can
; access a help topic.  Thus we will tell the edit manager that we
; have 0 lines, so it will not call us to redraw the screen.

        mov     ax,iFileSize            ; get size of variable help
        test    HelpFlags,HLP_INHELP    ; are we recursively entering help?
        jz      ExitGHFS                ; no, return with what we got
        cmp     fMessageBox,0           ; are we in a message box?
        jz      ExitGHFS                ; no, return with what we got
        cCall   DrawDebugScr            ; make sure we are redrawn later
        xor     ax,ax                   ; return 0 lines
ExitGHFS:
cEnd



;***
;HelpWndProc - Window Proc for the help window
;
;Purpose:
;       This routine interprets all messages that are going to the help
;       window.  Those of interest is processes and returns a value.
;       Otherwize, it will pass the message on to EditFilterWndProc.
;
;       This routine implements the receiving end of the messaging system.
;       It must be able to be called recursively.
;
;Entry:
;       pwnd    - Ptr to window that is to receive the message
;       msg     - message
;       wParam  - Word parameter to message
;       lParamHi-\ Long word parameter to message, broken up for ease of use.
;       lParamLo-/
;
;Exit:
;       DX:AX - return value
;
;****

labelW  MessageTable                    ; list of addresses for our messages
        dw      UIOFFSET HelpBack
        dw      UIOFFSET CmdHelpNext
        dw      UIOFFSET DisplayHlpGeneric
        dw      UIOFFSET GetHelpCurTopic
        dw      UIOFFSET GetHelpNextTopic
        dw      UIOFFSET ChngHelpCurTopic
        dw      UIOFFSET RestoreHelpTopic
        dw      UIOFFSET GetHelpTitle
        dw      UIOFFSET GetHelpLine
        dw      UIOFFSET GetHelpLineAttr
        dw      UIOFFSET GetHelpFileSize

        .errnz  (($ - MessageTable) / 2) - NUM_HELPMSG

cProc   HelpWndProc,<PUBLIC,FAR>,<SI>
parmW   pwnd
parmW   msg
parmW   wParam
parmW   lParamHi
parmW   lParamLo
cBegin
        inc     fHelpAlloc              ; set recursion flag


        mov     cx,wParam               ; cache in a register for speed
        mov     ax,msg                  ; cache in a register for speed

        mov     bx,ax
        sub     bx,WM_FIRSTHELPMSG      ; is it one of our special msgs?
        cmp     bx,NUM_HELPMSG
        jae     NotInTable              ; no, it is not in table
        shl     bx,1                    ; make into a word index
        mov     dx,lParamHi             ; Make lParam accessable
        mov     ax,lParamLo
        call    CS:MessageTable[bx]     ; and go do the code
        jmp     WndProcExit_2           ; return with code from the call

NotInTable:
        cmp     ax,WM_LBUTTONDBLCLK
        jne     CheckSETFOCUS

DbAssertRel     pwnd,e,<OFFSET DGROUP:wndHelp>,UI,<HelpWndProc:mouse message not for Help window>
        mov     bx,lParamLo
        mov     al,bh                   ; al = screen relative row number
        cbw                             ; ax = screen relative row number
        add     ax,EfHelp.EF_pdCur_olnTop ; ax = file relative row number
        xor     bh,bh                   ; bx = screen relative column number
        add     bx,EfHelp.EF_pdCur_obleft ; bx = file relative column number
        push    BX                      ; push first parameter
SelectHotLinkTrue:
        push    ax

; a-emoryh - Don't beep in QHelp mode when can't find hotlink
;            Do we really want to not beep, though?
        mov     ax, 1                   ; assume do beep
        test    cmdSwitches,CMD_SW_QHELP ; /QHELP viewer?
        jz      hwpDoBeep
        xor     ax, ax                  ; Qhelp mode, so clear beep flag
hwpDoBeep:
        push    ax

;; Old line
;        push    sp

        cCall   SelectHotLink           ; select hot link, allow BEEP
        jmp     WndProcExit

CheckSETFOCUS:
        cmp     ax,WM_SETFOCUS
        jne     CheckSETBOOKMARK
        PUSHI   ax,<OFFSET DGROUP:wndHelp> ; borrow the SETFOCUS call to
        cCall   DoStatusMsg             ; update the status line message
        jmp     Short DefaultCase       ; Pass call onto the edit mgr.

CheckSETBOOKMARK:
        cmp     ax,WM_SETBOOKMARK
        jne     CheckGOTOBOOKMARK
        sub     cx,'0'                  ; CX = wParam - '0'
        cCall   SetBookMark,<CX>
        jmp     short WndProcExit

CheckGOTOBOOKMARK:
        cmp     ax,WM_GOTOBOOKMARK
        jne     CheckCHAR
        sub     cx,'0'                  ; CX = wParam - '0'
        cCall   GotoBookMark,<CX>
        jmp     short WndProcExit

CheckCHAR:
        cmp     ax,WM_CHAR
        jne     DefaultCase
        cmp     cx,09h                  ; is the character a TAB
        jne     NotTab                  ; no, go check for ENTER
        xor     ax,ax                   ; assume not shifted
        test    lParamHi,KK_SHIFT       ; is a shift key pressed?
        jz      GotoHotLink             ; no, use a value of 0
        dec     ax                      ; use -1
GotoHotLink:
        cCall   NextHotLink,<AX>
        jmp     short WndProcExit

NotTab:
        cmp     cx,0dh                  ; is the character an ENTER?
        jne     NotEnter                ; no, go test for another char
        cCall   GetEditColumn           ; AX = file relative column number
        push    ax
        cCall   GetEditLine             ; AX = file relative line number
        jmp     SelectHotLinkTrue       ; select hot link (AX = line)

NotEnter:                               ; check for legal characters
        cmp     cx,7fh                  ; is it a backspace?
        je      DefaultCase             ; yes,  let EditMgr have it
        or      ch,ch                   ; is it a virtual key
        jne     DefaultCase             ; yes, EditMgr will handle it
        cmp     cx,' '                  ; is it a control character
        jb      DefaultCase             ; yes, let EditMgr have it
        cCall   GetEditMgrState         ; Ctrl+Q or Ctrl+K active?
        or      ax,ax                   ; (non-zero if so)
        jnz     DefaultCase             ; yes, EditMgr will handle this
        cCall   toupper,<wParam>        ; convert parameter to uppercase
        cmp     ax,wParam               ; was it already upper?
        jne     GotoHotLink             ; no, goto hotlink
        neg     ax                      ; yes, tell hotlink to look backwards
        jmp     GotoHotLink             ; goto hotlink.

DefaultCase:
        cCall   EditFilterWndProc,<pwnd,msg,wParam,lParamHi,lParamLo>
        jmp     short WndProcExit_2
WndProcExit:
        xor     ax,ax                   ; return 0L
        cwd
WndProcExit_2:
        dec     fHelpAlloc              ; release allocation lock
        cCall   CloseCurHelpFile        ; close the current help file
cEnd

sEnd    UI
        end
