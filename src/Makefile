all : ../tigr.c ../lib/github.com/diku-dk/sml-tigr/clib/tigr.c ../lib/github.com/diku-dk/sml-tigr/clib/tigr.h

.PHONY : ../tigr.c
.PHONY : ../lib/github.com/diku-dk/sml-tigr/clib/tigr.c
.PHONY : ../lib/github.com/diku-dk/sml-tigr/clib/tigr.h

tigr_font.h : font.png incbin.pl
	perl incbin.pl $@ $< tigr_font

../tigr.c : tigr_font.h bundle.pl
	perl bundle.pl $@ tigr_amalgamated.c

../lib/github.com/diku-dk/sml-tigr/clib/tigr.c : ../tigr.c
	cp $< $@

../lib/github.com/diku-dk/sml-tigr/clib/tigr.h : ../tigr.h
	cp $< $@
