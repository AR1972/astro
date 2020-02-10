rem 
rem     Microsoft Confidential
rem     Copyright (C) Microsoft Corporation 1991
rem     All Rights Reserved.
rem 

rem this is writen so that STRIPZ will work with NMK

rem out -f ..\inc\bdsize.inc
getsize
nmake
rem in -f -c "update size" ..\inc\bdsize.inc
