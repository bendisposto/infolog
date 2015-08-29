.PHONY: run
#PROBPATH=$(PROB_HOME)
PROBPATH=/Users/leuschel/git_root/prob_prolog
run:
	@echo "Starting ProB UI"
	export PROB_HOME=$(PROBPATH) ; rlwrap sicstus -l infolog_tk_gui.pl --goal "analyze(['$(PROBPATH)/src/prob_cli.pl']),infolog_ui."