SHELL := /bin/bash
.PHONY: all test edoc dialyzer clean

all:
	(cd src;$(MAKE))

test: clean
	(cd src;$(MAKE) TEST=true EUNIT=true)
	(erl -pa ./ebin -eval "eunit:test(\"./ebin\", [verbose]), init:stop()")

comm_test:
	(mkdir -p ./test/log)
	(run_test -DTEST -logdir `pwd`/test/log -include `pwd`/include -pa `pwd`/ebin -cover test/motown.coverspec -dir .)

edoc: 
	(mkdir -p ./edoc)
	(cd src; $(MAKE) edoc)

tags :
	(ctags -R .)

clean:
	(cd src;$(MAKE) clean)
