
LD=cc -arch x86_64
LDFLAGS=-framework OpenGL -framework Cocoa

.PHONY: lib
lib: libtigr.so

%.o: %.c
	$(CC) -arch x86_64 -c -o $@ -I. $<

libtigr.so: tigr.o tigr2.o
	$(LD) -shared -o $@ $^ $(LDFLAGS)

.PHONY: clean
clean:
	rm -rf *~ *.so *.o
