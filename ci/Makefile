CFLAGS += -I..
ifeq ($(OS),Windows_NT)
	LDFLAGS += -s -lopengl32 -lgdi32
	EXT = .exe
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		LDFLAGS += -framework OpenGL -framework Cocoa -L. -ltigr
		CFLAGS += -arch x86_64
	else ifeq ($(UNAME_S),Linux)
		LDFLAGS += -s -lGLU -lGL -lX11 -L. -ltigr
	endif
endif

.PHONY: all
all:
	$(MAKE) -C .. prepare
	$(MAKE) ci

ci$(EXT): ci.c libtigr.so
	$(CC) $< -o $@$(EXT) $(CFLAGS) $(LDFLAGS)

libtigr.so: ../lib/github.com/diku-dk/sml-tigr/clib/libtigr.so
	cp -p $< $@

.PHONY: clean
clean:
	rm -rf *~ ci *.o *.so
