M001 13-Aug-91	DLB expand.c	Bug #1879: Added the following functionality:
						 expand.h
						 expmsgs.h
						 sulib.h
						 dosdir.asm
						 makefile

	1) Do not allow source file to overwrite itself.

	2) If source file cannot be found, try file name with 3rd character of
		extension set = '_'.

	3) Display status messages as each file is expanded (like COPY, but
		display status even if just expanding 1 file):
			A:\EGA.SY_ -> C:\DOS\EGA.SYS
		Each message will appear on a new line, so if multiple files are
		expanded, the screen will scroll.

	4) Number of input parameters:
		a) = 0: Prompt for source and destination file/path
		b) = 1: Prompt for destination file/path
		c) = 2:	Source must be file name
					Destination can be file name or existing directory name
		d) > 2:	First parameters (sources) must be file names
					Last parameter (destination) must be existing directory name

	5) Destination file name:
		a) Can be explicitly specified when only 1 file is being expanded.
		b) When no destination file name is specified, the source file name is
			used as specified on command-line (i.e. if
			"EXPAND A:\EGA.SYS  C:\DOS" finds A:\EGA.SY_, it will display
			the status message "A:\EGA.SY_ -> C:\DOS\EGA.SYS" and create the
			file C:\DOS\EGA.SYS).
