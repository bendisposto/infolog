ifndef PROBPATH
    $(error PROBPATH is undefined)
endif

ifdef PROBPATH
    ABSOLUTE_PROB_PATH=$(realpath $(PROBPATH))
endif


.PHONY: test all all_cli all_tk server updatedb
PROLOG_FLAGS=

updatedb:
	make all
infolog.pdf: infolog.dot Makefile
	#sfdp -Tpdf -x -Goverlap=scale <infolog.dot >infolog.pdf
	#sfdp -Tpdf -x -Goverlap=prism <infolog.dot >infolog.pdf
	#fdp <infolog.dot >infolog.pdf
	twopi <infolog.dot >infolog.pdf
	#circo <infolog.dot >infolog.pdf
	open infolog.pdf
test:
	export PROB_HOME=$(ABSOLUTE_PROB_PATH)
	rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/tools.pl')."
test-verbose:
	export PROB_HOME=$(ABSOLUTE_PROB_PATH)
	sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/tools.pl'),export(user_output)."
alltk: prolog-analyzer/tcltk_calls.pl
	export PROB_HOME=$(ABSOLUTE_PROB_PATH) ; rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_tcltk.pl'])."
allcli:
	export PROB_HOME=$(ABSOLUTE_PROB_PATH) ; rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_cli.pl'])."
all: prolog-analyzer/tcltk_calls.pl
	@echo "analyzing ProB Tcl/Tk and probcli together"
	export PROB_HOME=$(ABSOLUTE_PROB_PATH) ; rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_tcltk.pl','$(PROBPATH)/src/prob_cli.pl'],'prolog-analyzer/meta_user_pred_cache.pl'), update_problem_db('$(PROBPATH)/src/infolog_problem_db.pl')."

infolog_problems.csv:  prolog-analyzer/*.pl prolog-analyzer/meta_user_pred_cache.pl prolog-analyzer/tcltk_calls.pl
	@echo "Generating CSV FIle"
	export PROB_HOME=$(ABSOLUTE_PROB_PATH) ; rlwrap sicstus -l prolog-analyzer/analyzer.pl --goal "analyze(['$(PROBPATH)/src/prob_tcltk.pl','$(PROBPATH)/src/prob_cli.pl'],'prolog-analyzer/meta_user_pred_cache.pl'), lint_to_csv_file('infolog_problems.csv')."

infolog.edn:  prolog-analyzer/*.pl prolog-analyzer/meta_user_pred_cache.pl prolog-analyzer/tcltk_calls.pl prolog-analyzer/java_calls.pl
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
	rm -f prolog-analyzer/meta_user_pred_cache.pl
	echo ':- dynamic meta_user_pred/3.' > prolog-analyzer/meta_user_pred_cache.pl
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

prolog-analyzer/java_calls.pl: Makefile
	@echo "Extracting Prolog calls from ProB 2.0 Java API"
	@echo ":- dynamic java_call/2." > prolog-analyzer/java_calls.pl
	find $(PROB2_PATH) -type f \( -iname \*.java -o -iname \*.groovy \) -exec perl -ne'print "java_call($$1,\"'{}'\").\n" if /.*?PROLOG_COMMAND_NAME\s*=\s*\"(.*)\"/' {} \; >> prolog-analyzer/java_calls.pl

clean_tcltk:
	rm prolog-analyzer/tcltk_calls.ack
prolog-analyzer/tcltk_calls.ack: Makefile
	 #grep -o 'prolog\s\"\?\([a-zA-Z_]*\)' $(PROBPATH)/tcl/*.tcl 
	 ack -o    '(?<=prolog)\s+("?(\{|\()?)([[a-zA-Z0-9_:\s]*)' $(ABSOLUTE_PROB_PATH)/tcl/*.tcl > prolog-analyzer/tcltk_calls.ack
	 ack -o '(?<=prologmnf)\s+("?(\{|\()?)([[a-zA-Z0-9_:\s]*)' $(ABSOLUTE_PROB_PATH)/tcl/*.tcl >> prolog-analyzer/tcltk_calls.ack
	 ack -o '(?<=prologmnfi)\s+("?(\{|\()?)([[a-zA-Z0-9_:\s]*)' $(ABSOLUTE_PROB_PATH)/tcl/*.tcl >> prolog-analyzer/tcltk_calls.ack

prolog-analyzer/tcltk_calls.pl: prolog-analyzer/tcltk_calls.ack prolog-analyzer/tcltk_call_importer.pl
	@echo "Importing Calls from ProB Tcl/Tk interface"
	sicstus -l prolog-analyzer/tcltk_call_importer.pl --goal "process_file('prolog-analyzer/tcltk_calls.ack'),generate_prolog_file('prolog-analyzer/tcltk_calls.pl'),halt."

cp_tcltk_calls: prolog-analyzer/tcltk_calls.pl
	@echo "Copying tcltk_calls.pl to ProB src/tcltk directory (for BBEdit search)"
	cp prolog-analyzer/tcltk_calls.pl $(ABSOLUTE_PROB_PATH)/src/tcltk/

prolog-analyzer/meta_preds.pl: prolog-analyzer/meta_pred_generator.pl
	sicstus -l prolog-analyzer/meta_pred_generator.pl --goal "tell('prolog-analyzer/meta_preds.pl'),gen,told,halt."
