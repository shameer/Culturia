all:
	@echo "héllo!"

# reset:
# 	@rm /tmp/wt/ -rf; mkdir /tmp/wt

check: 
	@guile -L `pwd` tests.scm

# check2: reset
# 	@/home/amirouche/src/guile/guile/guile/meta/guile -L `pwd` example.scm
