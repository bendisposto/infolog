.PHONY: test all all_cli
PROLOG_FLAGS=
#PROBPATH=$(PROB_HOME)
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
alltk:
	export PROB_HOME=$(PROBPATH) ; rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_tcltk.pl'])."
allcli:
	export PROB_HOME=$(PROBPATH) ; rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_cli.pl'])."
all:
	export PROB_HOME=$(PROB_PATH)
	@echo "analyzing ProB Tcl/Tk and probcli together; you will get redefinition warnings !"
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_tcltk.pl','$(PROBPATH)/src/prob_cli.pl'])."

prolog-analyzer/tcltk_calls.ack:
	 #grep -o 'prolog\s\"\?\([a-zA-Z_]*\)' $(PROBPATH)/tcl/*.tcl
	 ack -o '(?<=prolog)\s+("?)([[a-zA-Z0-9_:]*)' $(PROBPATH)/tcl/*.tcl > prolog-analyzer/tcltk_calls.ack
	 ack -o '(?<=prologmnf)\s+("?)([[a-zA-Z0-9_:]*)' $(PROBPATH)/tcl/*.tcl >> prolog-analyzer/tcltk_calls.ack

prolog-analyzer/tcltk_calls.pl: prolog-analyzer/tcltk_calls.ack prolog-analyzer/tcltk_call_importer.pl
	@echo "Importing Calls from ProB Tcl/Tk interface"
	sicstus -l prolog-analyzer/tcltk_call_importer.pl --goal "process_file('prolog-analyzer/tcltk_calls.ack'),generate_prolog_file('prolog-analyzer/tcltk_calls.pl'),halt."

test_cli_source:
	@echo "Running infolog_cli from source (works; but probably not useful)"
	export PROB_HOME=$(PROBPATH) ; rlwrap sicstus -l prolog-analyzer/infolog_cli.pl --goal "infolog_start_cli,halt." -- '$(PROBPATH)/src/prob_tcltk.pl'

infolog_cli.sav: prolog-analyzer/*.pl
	sicstus $(PROLOG_FLAGS) -l prolog-analyzer/infolog_cli.pl --goal "save_program('infolog_cli.sav'),halt."
infolog_cli: infolog_cli.sav
	@echo "Building infolog_cli (infolog_cli DOES NOT WORK compiled !!!)"
	spld $(spld_copts) --static --output infolog_cli --resources=./infolog_cli.sav=/infolog_cli.sav
test_cli: infolog_cli
	@echo "compiled infolog_cli DOES NOT WORK yet !!!"
	export PROB_HOME=$(PROBPATH) ; infolog_cli $(PROBPATH)/src/prob_tcltk.pl
	
databse.clj:
	export PROB_HOME=$(PROB_PATH)
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/prob_tcltk.pl', 'database.clj')."
prolog-analyzer/meta_preds.pl: prolog-analyzer/meta_pred_generator.pl
	sicstus -l prolog-analyzer/meta_pred_generator.pl --goal "tell('prolog-analyzer/meta_preds.pl'),gen,told,halt."