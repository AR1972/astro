;   Command.com initialization messages.
;
;   See resmsg.equ for macro definition.

    msg BadVerMsg,   <"Incorrect DOS version",CR,LF>
    msg OutEnvMsg,   <"Out of environment space",CR,LF>
    msg     CopyrightMsg,<CR,LF,CR,LF,"Microsoft(R) MS-DOS(R) Version 6",CR,LF,"             (C)Copyright Microsoft Corp 1981-1993.",CR,LF>
    msg BadComLkMsg, <"Specified COMMAND search directory bad",CR,LF>
    msg BadComAccMsg,<"Specified COMMAND search directory bad, access denied",CR,LF>

    msg Help1Msg  <"Starts a new copy of the MS-DOS Command Interpreter.",CR,LF,CR,LF>
    msg Help2Msg  <"COMMAND [[drive:]path] [device] [/E:nnnnn] [/P] [/[C|K] command] [/MSG]",CR,LF,CR,LF>
    msg Help3Msg  <"  [drive:]path    Specifies the directory containing COMMAND.COM file.",CR,LF>
    msg Help4Msg  <"  device          Specifies the device to use for command input and output.",CR,LF>
    msg Help5Msg  <"  /E:nnnnn        Sets the initial environment size to nnnnn bytes.",CR,LF>
    msg Help6Msg  <"  /P              Makes the new Command Interpreter permanent (can't exit).",CR,LF>
    msg Help7Msg  <"  /C command      Executes the specified command and returns.",CR,LF>
    msg Help8Msg  <"  /K command      Executes the specified command and continues running.",CR,LF>
    msg Help9Msg  <"  /MSG            Stores all error messages in memory (requires /P as well).",CR,LF>

HelpMsgs    dw  RESGROUP:Help1Msg,RESGROUP:Help2Msg
        dw  RESGROUP:Help3Msg,RESGROUP:Help4Msg
        dw  RESGROUP:Help5Msg,RESGROUP:Help6Msg
        dw  RESGROUP:Help7Msg,RESGROUP:Help8Msg
        dw  RESGROUP:Help9Msg,0

    public  HelpMsgs


