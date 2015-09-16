ifndef PROBPATH
    $(error PROBPATH is undefined)
endif

ifdef PROBPATH
    ABSOLUTE_PROB_PATH=$(realpath $(PROBPATH))
endif


.PHONY: test all all_cli server
PROLOG_FLAGS=

test:
	export PROB_HOME=$(ABSOLUTE_PROB_PATH)
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/tools.pl')."
test-verbose:
	export PROB_HOME=$(ABSOLUTE_PROB_PATH)
	sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/tools.pl'),export(user_output)."
test2:
	export PROB_HOME=$(ABSOLUTE_PROB_PATH)
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/b_interpreter_check.pl')."
test3:
	export PROB_HOME=$(ABSOLUTE_PROB_PATH)
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/kernel_ordering.pl')."
alltk: prolog-analyzer/tcltk_calls.pl
	export PROB_HOME=$(ABSOLUTE_PROB_PATH) ; rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_tcltk.pl'])."
allcli:
	export PROB_HOME=$(ABSOLUTE_PROB_PATH) ; rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_cli.pl'])."
all: prolog-analyzer/tcltk_calls.pl
	@echo "analyzing ProB Tcl/Tk and probcli together"
	export PROB_HOME=$(ABSOLUTE_PROB_PATH) ; rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_tcltk.pl','$(PROBPATH)/src/prob_cli.pl'],'prolog-analyzer/meta_user_pred_cache.pl')."

infolog_problems.csv:  prolog-analyzer/*.pl prolog-analyzer/meta_user_pred_cache.pl prolog-analyzer/tcltk_calls.pl
	@echo "Generating CSV FIle"
	export PROB_HOME=$(ABSOLUTE_PROB_PATH) ; rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_tcltk.pl','$(PROBPATH)/src/prob_cli.pl'],'prolog-analyzer/meta_user_pred_cache.pl'), lint_to_csv_file('infolog_problems.csv')."

infolog.edn:  prolog-analyzer/*.pl prolog-analyzer/meta_user_pred_cache.pl prolog-analyzer/tcltk_calls.pl
	@echo "Generating Data for website"
	export PROB_HOME=$(ABSOLUTE_PROB_PATH) ; rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_tcltk.pl','$(PROBPATH)/src/prob_cli.pl'],'prolog-analyzer/meta_user_pred_cache.pl'),  export_to_clj_file('resources/public/infolog.edn'), halt."

indy.edn:
	@echo "Generating indentation analysis"
	rm -f resources/public/indy.edn
	touch resources/public/indy.edn
	@echo "{:complexity [" >> resources/public/indy.edn
	time find $(ABSOLUTE_PROB_PATH) -name *.pl -exec java -jar analyzers/indy.jar {} + >> resources/public/indy.edn
	@echo "]}" >> resources/public/indy.edn

clean:
	rm -f infolog_problems*.csv
	rm -f resources/public/infolog.edn
	rm -f resources/public/indy.edn

ui:
	@echo "Compiling UI"
	chmod +x lein
	./lein clean
	./lein cljsbuild once min

run_server:
	@echo "Starting Python Simpleserver"
	pushd resources/public; python -m SimpleHTTPServer; popd

server: ui infolog.edn indy.edn run_server

prolog-analyzer/tcltk_calls.ack: Makefile
	 #grep -o 'prolog\s\"\?\([a-zA-Z_]*\)' $(PROBPATH)/tcl/*.tcl
	 ack -o    '(?<=prolog)\s+("?(\{|\()?)([[a-zA-Z0-9_:]*)' $(ABSOLUTE_PROB_PATH)/tcl/*.tcl > prolog-analyzer/tcltk_calls.ack
	 ack -o '(?<=prologmnf)\s+("?(\{|\()?)([[a-zA-Z0-9_:]*)' $(ABSOLUTE_PROB_PATH)/tcl/*.tcl >> prolog-analyzer/tcltk_calls.ack

prolog-analyzer/tcltk_calls.pl: prolog-analyzer/tcltk_calls.ack prolog-analyzer/tcltk_call_importer.pl
	@echo "Importing Calls from ProB Tcl/Tk interface"
	sicstus -l prolog-analyzer/tcltk_call_importer.pl --goal "process_file('prolog-analyzer/tcltk_calls.ack'),generate_prolog_file('prolog-analyzer/tcltk_calls.pl'),halt."

prolog-analyzer/meta_preds.pl: prolog-analyzer/meta_pred_generator.pl
	sicstus -l prolog-analyzer/meta_pred_generator.pl --goal "tell('prolog-analyzer/meta_preds.pl'),gen,told,halt."
