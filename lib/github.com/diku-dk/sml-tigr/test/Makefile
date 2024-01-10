PROGNAME=hello
MLKIT=mlkit
LIBS=
ifeq ($(OS),Windows_NT)
	LDFLAGS += -s -lopengl32 -lgdi32
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		LDFLAGS += -framework OpenGL -framework Cocoa -L. -ltigr
	else ifeq ($(UNAME_S),Linux)
		LDFLAGS += -s -lGLU -lGL -lX11 -ltigr
		LIBS += -libs 'm,c,dl,GLU,GL,X11,tigr'
	endif
endif

.PHONY: all
all: $(PROGNAME).exe

$(PROGNAME).exe: $(PROGNAME).mlb $(PROGNAME).sml libtigr.so
	MLCOMP=mlkit $(MLKIT) -ldexe '$(CC) $(LDFLAGS)' $(LIBS) -o $@ $<

libtigr.so: ../clib/libtigr.so
	cp $< $@

.PHONY: clean
clean:
	rm -rf *~ *.exe MLB libtigr.so
