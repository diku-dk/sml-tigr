
.PHONY: prepare
prepare: lib/github.com/diku-dk/sml-random
	$(MAKE) -C src
	$(MAKE) -C lib/github.com/diku-dk/sml-tigr/clib lib

.PHONY: test
test: prepare
	$(MAKE) -C lib/github.com/diku-dk/sml-tigr/test
	$(MAKE) -C ci ci && (cd ci;  ./ci 1)
	$(MAKE) -C sml-examples/hello
	$(MAKE) -C sml-examples/flags
	$(MAKE) -C sml-examples/mandel

.PHONY: clean
clean:
	rm -rf *~ *.so *.o
	$(MAKE) -C lib/github.com/diku-dk/sml-tigr clean
	$(MAKE) -C ci clean
	$(MAKE) -C sml-examples/hello clean
	$(MAKE) -C sml-examples/flags clean
	$(MAKE) -C sml-examples/mandel clean

lib/github.com/diku-dk/sml-random:
	smlpkg sync
