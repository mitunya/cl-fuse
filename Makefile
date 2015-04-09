
# sbcl
LISP=sbcl
FLAGS=--non-interactive --load

# ccl
#  ccl --batch --load launch.lisp -e '(quit)'

all:
	$(LISP) $(FLAGS) launch.lisp

clean:
	rm -f fuse-grovel-result.c
	rm -f fuse-grovel-result.grovel-tmp.lisp
	rm -f fuse-grovel-result
	rm -f libfuse-launcher.so
	rm -f libfuse-launcher.dylib
