PROGNAME=hello
MLKIT=mlkit
LIBS=
ifeq ($(OS),Windows_NT)
	LDFLAGS += -s -lopengl32 -lgdi32
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		LDFLAGS += -arch x86_64 -framework OpenGL -framework Cocoa -L. -ltigr
	else ifeq ($(UNAME_S),Linux)
		LDFLAGS += -L.
		LIBS += -libs 'm,c,dl,GLU,GL,X11,tigr'
	endif
endif

.PHONY: all
all: $(PROGNAME).exe

$(PROGNAME).exe: $(PROGNAME).mlb $(PROGNAME).sml libtigr.so
	MLCOMP=mlkit $(MLKIT) -ldexe '$(CC) $(LDFLAGS)' $(LIBS) -o $@ $<

libtigr.so: ../../lib/github.com/diku-dk/sml-tigr/clib/libtigr.so
	cp $< $@

.PHONY: clean
clean:
	rm -rf *~ *.exe MLB libtigr.so
