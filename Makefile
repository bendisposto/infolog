PROBPATH=$(PROB_HOME)
testw:
	sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/kernel_waitflags.pl')."
test:
	sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/tools.pl')."
test-verbose:
	sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/tools.pl'),export(user_output)."
all:
	sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/prob_tcltk.pl', 'database.clj')."