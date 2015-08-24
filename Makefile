PROBPATH=$(PROB_HOME)
PROBPATH=/Users/leuschel/git_root/prob_prolog
test:
	export PROB_HOME=$(PROB_PATH)
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/tools.pl')."
test-verbose:
	export PROB_HOME=$(PROB_PATH)
	sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/tools.pl'),export(user_output)."
test2:
	export PROB_HOME=$(PROB_PATH)
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/b_interpreter_check.pl')."
test3:
	export PROB_HOME=$(PROB_PATH)
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/kernel_ordering.pl')."
all:
	export PROB_HOME=$(PROB_PATH)
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/prob_tcltk.pl', 'database.clj')."
prolog-analyzer/meta_preds.pl: prolog-analyzer/meta_pred_generator.pl
	sicstus -l prolog-analyzer/meta_pred_generator.pl --goal "tell('prolog-analyzer/meta_preds.pl'),gen,told,halt."