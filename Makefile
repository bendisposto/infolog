PROBPATH=$(PROB_HOME)
PROBPATH=/Users/leuschel/git_root/prob_prolog
.PHONY: test all all_cli
PROLOG_FLAGS=
spld_lopts= --LD
spld_copts=
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
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/prob_tcltk.pl')."
all_cli:
	export PROB_HOME=$(PROB_PATH)
	rlwrap sicstus -l prolog-analyzer/infolog_cli.pl --goal "infolog_start_cli,halt." -- '$(PROBPATH)/src/prob_tcltk.pl'
infolog_cli.sav: prolog-analyzer/*.pl
	@echo "Building infolog_cli.sav"
	sicstus $(PROB_PROLOG_FLAGS) -l prolog-analyzer/infolog_cli.pl --goal "save_program('infolog_cli.sav'),halt."
infolog_cli: infolog_cli.sav
	@echo "Building infolog_cli (DOES NOT WORK !!!)"
	spld $(spld_copts) --static --output infolog_cli --resources=./infolog_cli.sav=/infolog_cli.sav
test_cli: infolog_cli
	export PROB_HOME=$(PROB_PATH)
	infolog_cli $(PROBPATH)/src/prob_tcltk.pl
	
databse.clj:
	export PROB_HOME=$(PROB_PATH)
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/prob_tcltk.pl', 'database.clj')."
prolog-analyzer/meta_preds.pl: prolog-analyzer/meta_pred_generator.pl
	sicstus -l prolog-analyzer/meta_pred_generator.pl --goal "tell('prolog-analyzer/meta_preds.pl'),gen,told,halt."