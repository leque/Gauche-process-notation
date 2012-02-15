GOSH = gosh

check:
	$(GOSH) -I. test.scm

check.syntactical:
	$(GOSH) -I. test.syntactical.scm
