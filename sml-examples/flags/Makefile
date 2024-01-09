PROGNAME=flags

MLKIT=mlkit
LD=cc -arch x86_64

ifeq ($(OS),Windows_NT)
	LDFLAGS += -s -lopengl32 -lgdi32
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		LDFLAGS += -framework OpenGL -framework Cocoa -L. -ltigr
	else ifeq ($(UNAME_S),Linux)
		LDFLAGS += -s -lGLU -lGL -lX11 -ltigr
	endif
endif

.PHONY: all
all: $(PROGNAME).exe

$(PROGNAME).exe: $(PROGNAME).mlb $(PROGNAME).sml libtigr.so
	MLCOMP=mlkit $(MLKIT) -ldexe '$(LD) $(LDFLAGS)' -o $@ $<

libtigr.so: ../../lib/github.com/diku-dk/sml-tigr/clib/libtigr.so
	cp $< $@

.PHONY: clean
clean:
	rm -rf *~ *.exe MLB libtigr.so
