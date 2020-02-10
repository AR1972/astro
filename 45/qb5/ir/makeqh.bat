REM makeqh.bat - Make HELP.HLP *.QH file
REM 31-Jul-1992 bens    Initial version
REM
REM NOTE: polyawk is only on bens' machine, as he is the only one with
REM       a license.  We have to figure out how to get the help.qhf
REM       file generated correctly (no :F and :Z directives, and :N
REM       immediately follows .context tags), so that this step is
REM       not necessary.

polyawk -f mungeqh.awk help.qhf >help.qh
