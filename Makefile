SCHEME = ypsilon --sitelib=sitelib

.PHONY: check test

check: test

test:
	$(SCHEME) tests/sxml/serializer.scm
	$(SCHEME) tests/sxml/ssax.scm
