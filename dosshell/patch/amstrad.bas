REM This program patches dosshell.exe to fix a bug that happens
REM with some Amstrad Machines.
REM The program searches for the code sequence
REM     CMP WORD PTR[1AE2],+00    [833EE21A00]
REM     JZ +68                    [7468h] 
REM and replaces it with
REM     MUL WORD PTR[1AE2]        [F726E21A]
REM     DAA                       [27h]
REM     JZ +68                    [7468h] 

REM First verify that the dosshell.exe is in the current directory

ON ERROR GOTO errorcantfinddosshell
OPEN "dosshell.exe" FOR INPUT AS #1
CLOSE

REM Now Open the dosshell.exe and search for the code sequence

OPEN "dosshell.exe" FOR RANDOM AS #1 LEN = 1
PRINT "Scanning for code patch location...";
FIELD #1, 1 AS b$
FOR i = &H16000 TO &H40000
IF ((i MOD 500) = 0) THEN
        PRINT ".";
END IF
GET #1, i
IF (b$ = CHR$(&H83)) THEN
        GET #1, i + 1
        IF (b$ = CHR$(&H3E)) THEN
                GET #1, i + 2
                IF (b$ = CHR$(&HE2)) THEN
                        GET #1, i + 3
                        IF (b$ = CHR$(&H1A)) THEN
                                GET #1, i + 4
                                IF (b$ = CHR$(&H0)) THEN
                                        GET #1, i + 5
                                        IF (b$ = CHR$(&H74)) THEN
                                                GET #1, i + 6
                                                IF (b$ = CHR$(&H68)) THEN
                                                        PRINT "Found patch location at", HEX$(i)
                                                        GOTO foundpatch
                                                END IF
                                        END IF
                                END IF
                        END IF
                END IF
        END IF
END IF
NEXT i
PRINT ""
PRINT "Cannot find patch location."
END
foundpatch:
REM If we get here, we have found the patch location
PRINT ""
PRINT "Patching dosshell.exe"
CLOSE
OPEN "dosshell.exe" FOR RANDOM AS #1 LEN = 1

FIELD #1, 1 AS a$

LSET a$ = CHR$(&HF7)
PUT #1, i
LSET a$ = CHR$(&H26)
PUT #1, i + 1
LSET a$ = CHR$(&H27)
PUT #1, i + 4
CLOSE

END

errorcantfinddosshell:
        PRINT "Cannot find dosshell.exe."
        PRINT "dosshell.exe must be in the current (default) directory"









