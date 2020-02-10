del log
del err
redirect -e log make localSources -f ..\ir\makefile > log
make -n qb3.exe qb3sh.exe tshell.exe -f makefile -f ..\qb\makefile -f ..\ir\makefile >foo.bat
redirect -e err foo >> log
fgrep warning log err
fgrep error log err
