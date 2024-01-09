
.PHONY: prepare
prepare:
	$(MAKE) -C src
	$(MAKE) -C lib/github.com/diku-dk/sml-tigr/clib lib

.PHONY: test
test: prepare
	$(MAKE) -C lib/github.com/diku-dk/sml-tigr/test

.PHONY: clean
clean:
	rm -rf *~ *.so *.o
	$(MAKE) -C lib/github.com/diku-dk/sml-tigr clean
	$(MAKE) -C sml-examples/hello clean
