SOURCES := $(wildcard *.lisp) $(wildcard *.asd)
QL_LOCAL=$(PWD)/.quicklocal/quicklisp
LOCAL_OPTS=--noinform --noprint --no-sysinit --no-userinit
QL_OPTS=--load $(QL_LOCAL)/setup.lisp

all: sbin/init

deps:
	@sbcl $(LOCAL_OPTS) $(QL_OPTS) \
		--eval '(push "$(PWD)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :linit)' \
		--eval '(quit)'
	@touch $@

sbin:
	@mkdir -p sbin

sbin/init: $(SOURCES) $(QL_LOCAL)/setup.lisp deps sbin
	@sbcl $(LOCAL_OPTS) $(QL_OPTS) \
		--eval '(push "$(PWD)/" asdf:*central-registry*)' \
		--eval "(asdf:operate 'asdf:build-op :linit)" \
		--eval '(quit)'

.PHONY: clean install

install:
	cp sbin/init /sbin/init
	mkdir -p /lib/linit
	cp -R default/* /lib/linit/

clean:
	@rm -rf deps .quicklocal bin quicklisp.lisp

$(QL_LOCAL)/setup.lisp: quicklisp.lisp
	@sbcl --noinform --noprint --disable-debugger --no-sysinit --no-userinit \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_LOCAL)")' \
		--eval '(quit)'

quicklisp.lisp:
	@wget https://beta.quicklisp.org/quicklisp.lisp
	@echo '4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17 *quicklisp.lisp' | shasum -c -
