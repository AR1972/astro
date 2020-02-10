#;/*
#; *                      Microsoft Confidential
#; *                      Copyright (C) Microsoft Corporation 1991
#; *                      All Rights Reserved.
#; */
# from IBM source tree

COM=\COMMON

4208-CPY.OBJ:  $(COM)\COPYRIGH.INC 4208-cpy.asm 4208.mak
               ASM87 4208-CPY

4208-CPY.BIN: 4208-CPY.OBJ $(COM)\SETVER.BAT 4208.mak
        LINK 4208-CPY;
        EXE2BIN 4208-CPY

4208.CPI: 4208.BIN 4208-cpy.bin $(COM)\SETVER.BAT 4208.mak
        COPY /B 4208.BIN+4208-cpy.bin 4208.CPI
        TAG 4208.CPI
