# mungeqh.awk - Modify *.QH files for input to HLPMAKE2
#    1) Put :n commands immediately following corresponding .context
#    2) Remove :f and :z commands -- no use to QBASIC
#    4) Remove Search, Contents, Index, Next, Back buttons
#
# History:  24-Mar-1992 bens Initial version
#
BEGIN {
    cLine = 0;	# no history lines
}

/^:f/ { 	# Remove these lines (what is their purpose?)
  # print "*** skipped :f ***";
    next;
}

/^:z/ { 	# Remove these lines (no frozen section support)
  # print "*** skipped :z ***";
    next;
}

/^\.context/ {
  # print "*** saw .context ***";
    if ($0 == ".context idx") {
	print ".context helpQHIndexId";
    }
    else if ($0 == ".context cmdcontents") {
	print ".context helpQHContentsId";
    }
    cLine = 1;	# may be seeing history lines
    print $0;	# No hot spot, so no need to fix!
    next;
}

/^:n/ {
  # print "*** saw :n ***";
    print $0;	# No hot spot, so no need to fix!
    for (i=1; i<cLine; i++) # print history lines, if any
	print FixHotSpots(history[i]);
    cLine=0;	# no more history
    next;
}

{
    if (cLine > 0) {	# Collecting history lines
  #	print "*** collecting history line ***";
	history[cLine] = $0;
	cLine++;
    }
    else {
	print FixHotSpots($0);
    }
}

#***	FixHotSpots
#
function FixHotSpots(sz) {
  # 23-Apr-1992 davided cleaned up help.qhf, so these are not needed!
  # gsub(/<\\p/,"<\\p\\a",sz);
  # gsub(/\\p/,"\\p\\a",sz);
  # 03-Apr-1992 davided cleaned up help.qhf, so these are not needed!
  # sub(/\\i\\p\\aSearch\\vsrc\\v\\i\\p/,"",sz);
  # sub(/\\i\\p\\aIndex\\v@idx\\v\\i\\p/,"",sz);
  # sub(/\\i\\p\\aPrevious\\v!B\\v\\i\\p/,"",sz);
  # sub(/\\i\\p\\aNext\\v.*\\v\\i\\p/,"",sz);
  # sub(/\\i\\p\\aContents\\v@cmdcontents\\v\\i\\p/,"",sz);
    return sz;
}
