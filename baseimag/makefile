BAT_DIR=$(LANG_SRC)\$(COUNTRY)\baseimag

all: CopyFil            \
     MkDirs          

CopyFil:   tools\null    \
           tools\ync.exe \
           tools\imdump.exe
tools\null:
    if not exist tools\nul       md tools

tools\ync.exe:
     copy ..\tools\bin\ync.exe    tools

tools\imdump.exe:
     copy ..\tools\bin\imdump.exe tools

MkDirs:
     cd 12
     if not exist 12OEM1\nul      md 12OEM1
     if not exist 12OEM2\nul      md 12OEM2
     if not exist 12OEM3\nul      md 12OEM3
     if not exist 12OEM4\nul      md 12OEM4
     if not exist 12OEM5\nul      md 12OEM5
     if not exist 12OEM6\nul      md 12OEM6
     if exist mkoem12.bat attrib -r mkoem12.bat
     if exist mkoem12.bat del mkoem12.bat
     copy $(BAT_DIR)\MKOEM12.$(COUNTRY) MKOEM12.BAT
     mkoem12
     cd ..\144
     if not exist 144OEM1\nul     md 144OEM1
     if not exist 144OEM2\nul     md 144OEM2
     if not exist 144OEM3\nul     md 144OEM3
     if not exist 144OEM4\nul     md 144OEM4
     if not exist 144OEM5\nul     md 144OEM5
     if exist mkoem144.bat attrib -r mkoem144.bat
     if exist mkoem144.bat del mkoem144.bat
     copy $(BAT_DIR)\MKOEM144.$(COUNTRY) MKOEM144.BAT
     mkoem144
     cd ..
