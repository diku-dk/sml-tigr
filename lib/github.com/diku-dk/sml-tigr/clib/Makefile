CFLAGS += -I.
ifeq ($(OS),Windows_NT)
	LDFLAGS += -s -lopengl32 -lgdi32
	EXT = .exe
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		UNAME_M := $(shell uname -m)
		ifeq ($(UNAME_M),arm64)
		else
			LDFLAGS += -arch x86_64
			CFLAGS += -arch x86_64
		endif
		LDFLAGS += -framework OpenGL -framework Cocoa
	else ifeq ($(UNAME_S),Linux)
		CFLAGS += -fPIC
		LDFLAGS += -s -lGLU -lGL -lX11
	endif
endif

.PHONY: lib
lib: libtigr.so

%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) $<

libtigr.so: tigr.o tigr2.o
	$(CC) -shared -o $@ $^ $(LDFLAGS)

.PHONY: clean
clean:
	rm -rf *~ *.so *.o
