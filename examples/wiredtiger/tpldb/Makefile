all:
	@echo "Try: check, clean"

check: clean
	guile -L `pwd` tests.scm

clean:
	rm tmpdb -rf || true
