MSG_DIR=$(LANG_SRC)\$(COUNTRY)\install\basedata\12

ALL:Makefile.$(COUNTRY) MkFil

Makefile.$(COUNTRY): $(MSG_DIR)\makefile
     copy $(MSG_DIR)\makefile makefile.$(COUNTRY)

MkFil:
     $(MAKE) /A /F makefile.$(COUNTRY)
