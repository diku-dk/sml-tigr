
.PHONY: prepare
prepare:
	$(MAKE) -C clib

.PHONY: test
test: prepare
	$(MAKE) -C test

.PHONY: clean
clean:
	rm -rf *~ MLB
	$(MAKE) -C clib clean
	$(MAKE) -C test clean
