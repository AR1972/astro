;      SCCSID = @(#)error2.inc	8.11 87/03/23
;BREAK <CP/DOS error codes>

ERROR_INVALID_PROCID            EQU     303     ; invalid process id
ERROR_INVALID_PDELTA            EQU     304     ; invalid priority delta
ERROR_NOT_DESCENDANT            EQU     305     ; not descendant
ERROR_NOT_SESSION_MANAGER       EQU     306     ; requestor not session manager
ERROR_INVALID_PCLASS            EQU     307     ; invalid p class
ERROR_INVALID_SCOPE             EQU     308     ; invalid scope
ERROR_INVALID_THREADID          EQU     309     ; invalid thread id
ERROR_DOSSUB_SHRINK             EQU     310     ; can't shrink - MspSet
ERROR_DOSSUB_NOMEM              EQU     311     ; no memory - MspAlloc
ERROR_DOSSUB_OVERLAP            EQU     312     ; overlap - MspFree
ERROR_DOSSUB_BADSIZE            EQU     313     ; bad size parameter - MspAlloc or MspFree
ERROR_DOSSUB_BADFLAG            EQU     314     ; bad flag parameter - MspSet
ERROR_DOSSUB_BADSELECTOR        EQU     315     ; invalid MspSegment Selector
ERROR_MR_MSG_TOO_LONG           EQU     316     ; message too long for buffer
ERROR_MR_MID_NOT_FOUND          EQU     317     ; message id number not found
ERROR_MR_UN_ACC_MSGF            EQU     318     ; unable to access message file
ERROR_MR_INV_MSGF_FORMAT        EQU     319     ; invalid message file format
ERROR_MR_INV_IVCOUNT            EQU     320     ; invalid insertion variable count
ERROR_MR_UN_PERFORM             EQU     321     ; unable to perform function
ERROR_TS_WAKEUP                 EQU     322     ; unable to wake up
ERROR_TS_SEMHANDLE              EQU     323     ; user passed invalid system semaphore
ERROR_TS_NOTIMER                EQU     324     ; no times available
ERROR_TS_HANDLE                 EQU     326     ; invalid timer handle
ERROR_TS_DATETIME               EQU     327     ; date or time invalid
ERROR_SYS_INTERNAL              EQU     328     ; internal system error
ERROR_QUE_CURRENT_NAME          EQU     329     ; current name does not exist
ERROR_QUE_PROC_NOT_OWNED        EQU     330     ; current process does not own queue
ERROR_QUE_PROC_OWNED            EQU     331     ; current process owns queue
ERROR_QUE_DUPLICATE             EQU     332     ; duplicate name
ERROR_QUE_ELEMENT_NOT_EXIST     EQU     333     ; element does not exist
ERROR_QUE_NO_MEMORY             EQU     334     ; inadequate memory
ERROR_QUE_INVALID_NAME          EQU     335     ; invalid name
ERROR_QUE_INVALID_PRIORITY      EQU     336     ; invalid priority parameter
ERROR_QUE_INVALID_HANDLE        EQU     337     ; invalid queue handle
ERROR_QUE_LINK_NOT_FOUND        EQU     338     ; link not found
ERROR_QUE_MEMORY_ERROR          EQU     339     ; memory error
ERROR_QUE_PREV_AT_END           EQU     340     ; previous element was at end of queue
ERROR_QUE_PROC_NO_ACCESS        EQU     341     ; process does not have access to queues
ERROR_QUE_EMPTY                 EQU     342     ; queue is empty
ERROR_QUE_NAME_NOT_EXIST        EQU     343     ; queue name does not exist
ERROR_QUE_NOT_INITIALIZED       EQU     344     ; queues not initialized
ERROR_QUE_UNABLE_TO_ACCESS      EQU     345     ; unable to access queues
ERROR_QUE_UNABLE_TO_ADD         EQU     346     ; unable to add new queue
ERROR_QUE_UNABLE_TO_INIT        EQU     347     ; unable to initialize queues
ERROR_VIO_INVALID_MASK          EQU     349     ; invalid replacement mask
ERROR_VIO_PTR                   EQU     350     ; invalid pointer to parameter
ERROR_VIO_APTR                  EQU     351     ; invalid pointer to attribute
ERROR_VIO_RPTR                  EQU     352     ; invalid pointer to row
ERROR_VIO_CPTR                  EQU     353     ; invalid pointer to column
ERROR_VIO_LPTR                  EQU     354     ; invalid pointer to length
ERROR_VIO_MODE                  EQU     355     ; unsupported screen mode
ERROR_VIO_WIDTH                 EQU     356     ; invalid cursor width value
ERROR_VIO_ATTR                  EQU     357     ; invalid cursor attribute value
ERROR_VIO_ROW                   EQU     358     ; invalid row value
ERROR_VIO_COL                   EQU     359     ; invalid column value
ERROR_VIO_TOPROW                EQU     360     ; invalid toprow value
ERROR_VIO_BOTROW                EQU     361     ; invalid botrow value
ERROR_VIO_RIGHTCOL              EQU     362     ; invalid right column value
ERROR_VIO_LEFTCOL               EQU     363     ; invalid left column value
ERROR_SCS_CALL                  EQU     364     ; call issued by other than sm
ERROR_SCS_VALUE                 EQU     365     ; value is not for save or restore
ERROR_VIO_WAIT_FLAG             EQU     366     ; invalid wait flag setting
ERROR_VIO_UNLOCK                EQU     367     ; screen not previously locked
ERROR_SGS_NOT_SESSION_MGR       EQU     368     ; caller not session manager
ERROR_SMG_INVALID_SGID          EQU     369     ; invalid screen group id
ERROR_SMG_INVALID_SESSION_ID    EQU     369     ; invalid session id
ERROR_SMG_NOSG                  EQU     370     ; all screen groups in use
ERROR_SMG_NO_SESSIONS           EQU     370     ; no sessions available
ERROR_SMG_GRP_NOT_FOUND         EQU     371     ; screen group not found
ERROR_SMG_SESSION_NOT_FOUND     EQU     371     ; session not found
ERROR_SMG_SET_TITLE             EQU     372     ; title set by shell or parent cant be changed
ERROR_KBD_PARAMETER             EQU     373     ; invalid parameter to kbd
ERROR_KBD_NO_DEVICE             EQU     374     ; out of system handles
ERROR_KBD_INVALID_IOWAIT        EQU     375     ; invalid i/o wait specified
ERROR_KBD_INVALID_LENGTH        EQU     376     ; invalid length for keyboard
ERROR_KBD_INVALID_ECHO_MASK     EQU     377     ; invalid echo mode mask
ERROR_KBD_INVALID_INPUT_MASK    EQU     378     ; invalid input mode mask
ERROR_MON_INVALID_PARMS         EQU     379     ; invalid parameters to dos mon
ERROR_MON_INVALID_DEVNAME       EQU     380     ; invalid device name string
ERROR_MON_INVALID_HANDLE        EQU     381     ; invalid device handle
ERROR_MON_BUFFER_TOO_SMALL      EQU     382     ; buffer too small
ERROR_MON_BUFFER_EMPTY          EQU     383     ; buffer is empty
ERROR_MON_DATA_TOO_LARGE        EQU     384     ; data record too large
ERROR_MOUSE_NO_DEVICE           EQU     385     ; no mouse device attached
ERROR_MOUSE_INV_HANDLE          EQU     386     ; mouse device closed (invalid device handle)
ERROR_MOUSE_INV_PARMS           EQU     387     ; parameters invalid or out of range
ERROR_MOUSE_CANT_RESET          EQU     388     ; function assigned and cannot be reset
ERROR_MOUSE_DISPLAY_PARMS       EQU     389     ; parameters invalid for display mode
ERROR_MOUSE_INV_MODULE          EQU     390     ; module not found
ERROR_MOUSE_INV_ENTRY_PT        EQU     391     ; entry point not valid
ERROR_MOUSE_INV_MASK            EQU     392     ; function mask invalid
NO_ERROR_MOUSE_NO_DATA          EQU     393     ; no mouse data
NO_ERROR_MOUSE_PTR_DRAWN        EQU     394     ; pointer drawn
ERROR_INVALID_FREQUENCY         EQU     395     ; invalid frequency for beep
ERROR_NLS_NO_COUNTRY_FILE       EQU     396     ; can't find country.sys
ERROR_NLS_OPEN_FAILED           EQU     397     ; can't open country.sys
ERROR_NLS_NO_CTRY_CODE          EQU     398     ; country code not found
ERROR_NO_COUNTRY_OR_CODEPAGE    EQU     398     ; country code not found
ERROR_NLS_TABLE_TRUNCATED       EQU     399     ; table truncated, buffer too small
ERROR_NLS_BAD_TYPE              EQU     400     ; selector type does not exist
ERROR_NLS_TYPE_NOT_FOUND        EQU     401     ; selector type not in file
ERROR_VIO_SMG_ONLY              EQU     402     ; valid from session manager only
ERROR_VIO_INVALID_ASCIIZ        EQU     403     ; invalid asciiz length
ERROR_VIO_DEREGISTER            EQU     404     ; vio deregister disallowed
ERROR_VIO_NO_POPUP              EQU     405     ; popup not allocated
ERROR_VIO_EXISTING_POPUP        EQU     406     ; popup on screen (no wait)
ERROR_KBD_SMG_ONLY              EQU     407     ; valid from session manager only
ERROR_KBD_INVALID_ASCIIZ        EQU     408     ; invalid asciiz length
ERROR_KBD_INVALID_MASK          EQU     409     ; invalid replacement mask
ERROR_KBD_REGISTER              EQU     410     ; kbdregister disallowed
ERROR_KBD_DEREGISTER            EQU     411     ; kbdderegister disalloed
ERROR_MOUSE_SMG_ONLY            EQU     412     ; valid from session manager only
ERROR_MOUSE_INVALID_ASCIIZ      EQU     413     ; invalid asciiz length
ERROR_MOUSE_INVALID_MASK        EQU     414     ; invalid replacement mask
ERROR_MOUSE_REGISTER            EQU     415     ; mouse register disallowed
ERROR_MOUSE_DEREGISTER          EQU     416     ; mouse deregister disallowed
ERROR_SMG_BAD_ACTION            EQU     417     ; invalid action specified
ERROR_SMG_INVALID_CALL          EQU     418     ; init called more than once
ERROR_SCS_SG_NOTFOUND           EQU     419     ; new screen group #
ERROR_SCS_NOT_SHELL             EQU     420     ; caller is not shell
ERROR_VIO_INVALID_PARMS         EQU     421     ; invalid parms passed in
ERROR_VIO_FUNCTION_OWNED        EQU     422     ; save/restore already owned
ERROR_VIO_RETURN                EQU     423     ; non-destruct return (undo)
ERROR_SCS_INVALID_FUNCTION      EQU     424     ; not valid request for caller
ERROR_SCS_NOT_SESSION_MGR       EQU     425     ; caller not session manager
ERROR_VIO_REGISTER              EQU     426     ; VIO register disallowed
ERROR_VIO_NO_MODE_THREAD        EQU     427     ; No mode restore thread in SG
ERROR_VIO_NO_SAVE_RESTORE_THD   EQU     428     ; No save/rest thread in SG
ERROR_VIO_IN_BG                 EQU     429     ; Physical selector requested in background
ERROR_VIO_ILLEGAL_DURING_POPUP  EQU     430     ; Function not allowed during popup
ERROR_SMG_NOT_BASESHELL         EQU     431     ; caller is not the base shell
ERROR_SMG_BAD_STATUSREQ         EQU     432     ; invalid status requested
ERROR_QUE_INVALID_WAIT          EQU     433     ; nowait parameter out of bounds
ERROR_VIO_LOCK                  EQU     434     ; error returned from scrlock
ERROR_MOUSE_INVALID_IOWAIT      EQU     435     ; invalid parameters for IO wait
ERROR_VIO_INVALID_HANDLE        EQU     436     ; invalid vio handle
ERROR_VIO_ILLEGAL_DURING_LOCK   EQU     437     ; Function not allowed during screen lock
ERROR_VIO_INVALID_LENGTH        EQU     438     ; invalid vio length
ERROR_KBD_INVALID_HANDLE        EQU     439     ; invalid kbd handle
ERROR_KBD_NO_MORE_HANDLE        EQU     440     ; ran out of handles
ERROR_KBD_CANNOT_CREATE_KCB     EQU     441     ; unable to create kcb
ERROR_KBD_CODEPAGE_LOAD_INCOMPL EQU     442     ; unsuccessful codepage load
ERROR_KBD_INVALID_CODEPAGE_ID   EQU     443     ; invalid codepage id
ERROR_KBD_NO_CODEPAGE_SUPPORT   EQU     444     ; no codepage support
ERROR_KBD_FOCUS_REQUIRED        EQU     445     ; keyboard focus required
ERROR_KBD_FOCUS_ALREADY_ACTIVE  EQU     446     ; keyboard focus exists
ERROR_KBD_KEYBOARD_BUSY         EQU     447     ; keyboard busy
ERROR_KBD_INVALID_CODEPAGE      EQU     448     ; invalid codepage
ERROR_KBD_UNABLE_TO_FOCUS       EQU     449     ; focus attempt failed
ERROR_SMG_SESSION_NON_SELECT    EQU     450     ; session is not selectable
ERROR_SMG_SESSION_NOT_FOREGRND  EQU     451     ; parent/child session not foreground
ERROR_SMG_SESSION_NOT_PARENT    EQU     452     ; not parent of requested child
ERROR_SMG_INVALID_START_MODE    EQU     453     ; invalid session start mode
ERROR_SMG_INVALID_RELATED_OPT   EQU     454     ; invalid session start related option
ERROR_SMG_INVALID_BOND_OPTION   EQU     455     ; invalid session bond option
ERROR_SMG_INVALID_SELECT_OPT    EQU     456     ; invalid session select option
ERROR_SMG_START_IN_BACKGROUND   EQU     457     ; session started in background
ERROR_SMG_INVALID_STOP_OPTION   EQU     458     ; invalid session stop option
ERROR_SMG_BAD_RESERVE           EQU     459     ; reserved parameters not zero
ERROR_SMG_PROCESS_NOT_PARENT    EQU     460     ; session parent process already exists
ERROR_SMG_INVALID_DATA_LENGTH   EQU     461     ; invalid data length
ERROR_SMG_NOT_BOUND             EQU     462     ; parent not bound
ERROR_SMG_RETRY_SUB_ALLOC       EQU     463     ; retry request block allocation
ERROR_KBD_DETACHED              EQU     464     ; this call disallawed for detached pid
ERROR_VIO_DETACHED              EQU     465     ; this call disallawed for detached pid
ERROR_MOU_DETACHED              EQU     466     ; this call disallawed for detached pid
ERROR_VIO_FONT                  EQU     467     ; no font available to support mode
ERROR_VIO_USER_FONT             EQU     468     ; user font active
ERROR_VIO_BAD_CP                EQU     469     ; invalid code page specified
ERROR_VIO_NO_CP                 EQU     470     ; system displays don't support code page
ERROR_VIO_NA_CP                 EQU     471     ; current displays doesn't support code page
ERROR_INVALID_CODE_PAGE         EQU     472     ; invalid code page
ERROR_CPLIST_TOO_SMALL          EQU     473     ; code page list is too small
ERROR_CP_NOT_MOVED              EQU     474     ; code page not moved
ERROR_MODE_SWITCH_INIT          EQU     475     ; mode switch init error
ERROR_CODE_PAGE_NOT_FOUND       EQU     476     ; code page not found
ERROR_UNEXPECTED_SLOT_RETURNED  EQU     477     ; internal error
ERROR_SMG_INVALID_TRACE_OPTION  EQU     478     ; invalid start session trace indicator
ERROR_VIO_INTERNAL_RESOURCE     EQU     479     ; vio internal resource error
ERROR_VIO_SHELL_INIT            EQU     480     ; vio shell init error
ERROR_SMG_NO_HARD_ERRORS        EQU     481     ; no session manager hard errors

;
; intercomponent error codes (from 8000H or 32768)
;
ERROR_SWAPPER_NOT_ACTIVE        EQU     32768   ; swapper is not active
ERROR_INVALID_SWAPID            EQU     32769   ; invalid swap identifier
ERROR_IOERR_SWAP_FILE           EQU     32770   ; i/o error on swap file
ERROR_SWAP_TABLE_FULL           EQU     32771   ; swap control table is full
ERROR_SWAP_FILE_FULL            EQU     32772   ; swap file is full
ERROR_CANT_INIT_SWAPPER         EQU     32773   ; cannot initialize swapper
ERROR_SWAPPER_ALREADY_INIT      EQU     32774   ; swapper already initialized
ERROR_PMM_INSUFFICIENT_MEMORY   EQU     32775   ; insufficient memory
ERROR_PMM_INVALID_FLAGS         EQU     32776   ; invalid flags for phys. mem.
ERROR_PMM_INVALID_ADDRESS       EQU     32777   ; invalid address of phys. mem.
ERROR_PMM_LOCK_FAILED           EQU     32778   ; lock of storage failed
ERROR_PMM_UNLOCK_FAILED         EQU     32779   ; unlock of storage failed
ERROR_PMM_MOVE_INCOMPLETE       EQU     32780   ; move not completed
ERROR_UCOM_DRIVE_RENAMED        EQU     32781   ; drive name was renamed
ERROR_UCOM_FILENAME_TRUNCATED   EQU     32782   ; file name was truncated
ERROR_UCOM_BUFFER_LENGTH        EQU     32783   ; bad buffer length
ERROR_MON_CHAIN_HANDLE          EQU     32784   ; invalid chain handle - mon dh
ERROR_MON_NOT_REGISTERED        EQU     32785   ; monitor not registered
ERROR_SMG_ALREADY_TOP           EQU     32786   ; specified screen group is top
ERROR_PMM_ARENA_MODIFIED        EQU     32787   ; arena modified - phys mem
ERROR_SMG_PRINTER_OPEN          EQU     32788   ; printer open error on prtsc
ERROR_PMM_SET_FLAGS_FAILED      EQU     32789   ; update to arena header flags failed
