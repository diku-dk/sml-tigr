CFLAGS += -I.
ifeq ($(OS),Windows_NT)
	LDFLAGS += -s -lopengl32 -lgdi32
	EXT = .exe
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		LDFLAGS += -arch x86_64 -framework OpenGL -framework Cocoa
		CFLAGS += -arch x86_64
	else ifeq ($(UNAME_S),Linux)
		LDFLAGS += -s -lGLU -lGL -lX11 -L. -ltigr
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
