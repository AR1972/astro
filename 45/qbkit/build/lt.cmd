@echo off
helptree ..\files\qbasic.qh > qbasic.ht
helptree ..\files\edit.src > edit.ht
linktest
echo The results of linktest are in RESULTS.TXT.
