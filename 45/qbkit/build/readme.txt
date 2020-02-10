Notes on Building the Help Files for QBASIC and EDIT.

This build is set up for OS2.

To build microsoft version help files go into qbkit\build and type
build.  To build IBM versions, you must first build the microsoft
versions and then go to qbkit\build and type makeibm.
The output files for the microsoft version are QBASIC.HLP and EDIT.HLP,
for IBM version they are called IBMQB.HLP and IBMEDIT.HLP

The build is set up to expect the following directory structure




QBKIT
	FILES - the files for QBlang are copied in here for the build.
		no files are enlisted in this directory.
		You can delete it after a build if you wish.

	BUILD - Contains the files needed to build the help files.
		The most important ones are BUILD.CMD, MAKEIBM.CMD and
		GOODBLD.CMD.
		BUILD.CMD will produce ordinary help files.
		MAKEIBM.CMD will produce IBM versions of help files.
		GOODBLD.CMD is used to check in the files when the
		build was successful.

	BLD -	The build help files will be copied to this directory.
		It would be a good idea to keep copies of QBASIC.EXE 
		and EDIT.COM in this directory to test the files.

QBusa		The language files.  The files will be copied from here
		into QBKIT\FILES when the help files are being built.
		The directory HELPBLD is where the latest help files for
		the language will be maintained on SLM.
	
QBswe, QBdut, QBitn, QBfrn, QBger - These are the translated versions of
QBusa.  Remember that the STRINGS subdirectory within these directories
is for the EXE files and that all other files are for help alone.

You should enlist in the QBUSA and QBKIT directories.
