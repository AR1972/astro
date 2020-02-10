M000 - lf; bug #2452; 09/06/90 - to enable attrib to work with subdirectories,
I commented out three lines in DO_DIR routine and added one in REGULAR_ATTRIB.
The one added masks the attribute byte returned by GET_REG_ATTRIB with a mask
that turns off the subdirectory bit.  This is necessary because we later OR
the current attribute byte for the file with the attribute byte specified by
the user, and if the SUBDIR bit is turned on, trying to set the attribute
for that file using that byte will fail.  
The lines deleted were lines that said "if this is a subdirectory, don't
call ATTRIB (the part that really does the attribute stuff) on it."

M001 09/19/90 SMR	ATTRIB.C	Undid the M000 modification. And
			ATTRIB.H	implemented set attributes for
					Subdirectory entries only when they
					are explicitly specified and thru
					wild card specifications

M002 09/21/90 MD        proto.h         Corrected declarations of inmain() 
                                        and main()
