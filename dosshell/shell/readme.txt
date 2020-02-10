

	This is the Source to the DOSSHELL version 2 

	To build, use the build.bat batch file: "build clean"


Special Note to Internationalizers:
	Throughout the code we have added comments indicating all
	the places we are aware of which may require internationalization.
	The comment is "/* INTERNATIONALIZE HERE! [...] */" or
	in asm ";;;; INTERNATIONALIZE HERE! ..."
	It is almost certainly the case that we have missed places.

	In particular, dialogs should be modified with the CW dialog
	editor "de21.exe". These are the ".sdm",".hs",and ".des" files in
	shellh. To change strings in the dialog, open the ".des" file and
	click on items with text. Boxes surrounding the text or button
	may have to be expanded. Be certain that with buttons the box is
	resized appropriately so that it does not become lopsided!
