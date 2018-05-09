% (c) 2013-2018 Lehrstuhl fuer Softwaretechnik und Programmiersprachen,
% Heinrich Heine Universitaet Duesseldorf
% This file expands clause terms, runs some analysis on them,
% and writes the results to CSV or EDN files.

portray_message(informational, _).

:- use_module(escaper).
:- use_module(infolog_tools).
%:- use_module(clojure_exporter,[export_to_clj_file/1, export/1]).
:- use_module(library_modules).

:- use_module(library(lists)).
:- use_module(library(sets)).
:- use_module(library(terms)).
:- use_module(library(system)).
:- use_module(library(file_systems)).
:- use_module(library(codesio)).

% :- op(300, fy, ~~).

% THE DYNAMIC PREDICATES WHICH ARE PART OF THE ANALYSIS

:-  dynamic
    defined_module/2,% module(name,file) : there exists a :- module(Name, [...]). declaration
    predicate/2,     % predicate(module,name/arity)
    is_dynamic/2,    % is_dynamic(module,name/arity)
    is_public/2,     % is_public(module,name/arity)
    is_volatile/2,   % is_volatile(module,name/arity)
    is_chr_constraint/2,   % is_chr_constraint(module,name/arity)
    is_attribute/2,  % is_attribute(module,name/arity)
    is_foreign/2,    % is_foreign(module,name/arity)
    is_meta/2,       % is_meta(module:name/arity, meta_arguments)
    klaus/4,         % klaus(module,name/arity,  startline, endline)
    calling/6,       % calling(callingmodule,callingpredicate/callingarity, module,name/arity, startline, endline)
    meta_call/7,     % meta_call(callingmodule,callingpredicate/callingarity, VAR, ExtraArgs, ClauseHead, startline, endline)
    declared_mode/2, % declared_mode(module:name/arity, mode_arguments)
    is_exported/2,   % is_exported(module,name/arity)
    is_imported/3,   % is_imported(from_module,imported_module,imported_name/arity)
    depends_on/2,    % depends_on(local_module, imported_module) ;; local_module uses imported_module with use_module
    is_multifile/2,  % is_multifile(module,name/arity)
    is_blocking/2,   % is_blocking(module:name/arity, block_arguments)
    operator/4,      % operator(module:name/arity, priority, fixity, associativity)  ;; fixity : {prefix, infix, postfix}
    problem/2.       % problem(details,Loc)
:- dynamic
       clause_complexity/9,  % clause_complexity(Module,Name/Arity, NestingLevel, CallsInBody, StartLine, EndLine, Variables, Unifications, ExplicitUnifications)
       clause_halstead/8.    % clause_halstead(Module,Name/Arity, StartLine, EndLine, OperatorOcc, OperandOcc, DistinctOperators, DistinctOperands)

% optionally the following can also be stored:
:- dynamic
   next_stored_clause_nr/1, % nr of clauses stored
   stored_clause/4,  % stored_clause(Module,Head,Body,Layout)
   stored_call/4.    % stored_call(Module,Call,FromModule,Layout)
% The following line needs to be commented in so that these facts are generated
next_stored_clause_nr(0).
:- discontiguous
       is_documented/1, % predicate M:P/A is documented by a comment
       has_metapred_excuse/3, % predicate M:P/A has an excuse for using MetaPred
       has_documented_sideeffect/3. % predicate M:P/A has a documented side effect
:- dynamic
       has_sideeffect/2. % predicate M:P/A has a side effect

%%
% An 8-place variant of calling/7, because it's easier to export.
% @param CM the calling module
% @param CP the calling predicate's name
% @param CA the calling predicate's arity
% @param M the called module
% @param P the called predicate's name
% @param A the called predicate's arity
% @param SL start line of the call
% @param EL end line of the call
calling(CM,CP,CA,M,P,A,SL,EL) :- calling(CM,CP/CA,M,P/A,SL,EL).

%%
% A 10-place variant of clause_complexity/9, because it's easier to export.
% @param Module the containing module
% @param Name the predicate's name
% @param Arity the predicate's arity
% @param NestingLevel the determined nesting level
% @param CallsInBody the number of calls in the clause's body
% @param StartLine start line of the clause
% @param EndLine end line of the clause
% @param Variables number of Variables used in the clause
% @param Unif number of unifications done in the clause body
% @param EUnif number of explicit unifications done in the clause body (using =)
clause_complexity(Module,Name,Arity, NestingLevel, CallsInBody, StartLine, EndLine, Variables, Unif, EUnif) :- clause_complexity(Module,Name/Arity, NestingLevel, CallsInBody, StartLine, EndLine, Variables, Unif, EUnif).

%%
% Exports a couple of analysis results to a B file of the given name, using
% export_to_file/3.
% @param File the file name
export_to_b_file(File) :- export_to_file(b,File, [ depends_on/2, defined_module/2, is_library_module/1, calling/4]).

%%
% Exports a couple of analysis results to a Closure file of the given name,
% using export_to_file/2.
% @param File the file name
export_to_clj_file(File) :- export_to_file(clj,File).

%%
% Exports a couple of analysis results to a file of the given name in the given format, using export_to_file/3.
% @param Format the file format (b, clj)
% @param File the file name
export_to_file(Format,File) :-    List = [ depends_on/2, defined_module/2, calling/8, infolog_problem_flat/9, clause_complexity/10, clause_halstead/8, predicate/2, is_exported/2, is_dynamic/2, module_predicate_stats/4, pred_incalls_outcalls/4, has_undocumented_sideeffect/2],
                                  export_to_file(Format,File,List).

%%
% Exports the given predicates to the given file in the given format.
% @param Format the file format (b, clj)
% @param File the file name
% @param List a list of predicate/arity specifications
export_to_file(Format,File,List) :- start_analysis_timer(TT),
   open(File,write,S),
    call_cleanup((start_file(Format,S,List),
					maplist(export(Format,S), List),
					end_file(Format,S)),close(S)), stop_analysis_timer(TT).

%%
% Exports all solutions for a given predicate to an open file of the given format.
% @param Format the file format
% @param S the open file
% @param P/Arity the predicate that is to be exported
export(Format,S,P/Arity) :-
     start_pred(Format,S,P),
     functor(Call,P,Arity), Call =.. [_|Args],
     call(Call),
     start_tuple(Format,S),
     write_args(Args,Format,S),
     end_tuple(Format,S),
     fail.
export(Format,S,P/_Arity) :- end_pred(Format,S,P).
:- dynamic first_tuple/0.

%%
% Helper predicate that writes a predicate name to an open file.
% @param S the open file
% @param F/_ the predicate name and arity
prfunc(S,F/_) :- format(S,' ~w',[F]).

%%
% Write the header to an open export file.
% @param Format the file format (clj, b)
% @param S the open file
% @param List a list of predicate/arity specifications
start_file(clj,S,_) :- format(S,'{~n',[]).
start_file(b,S,L) :-
   format(S,'MACHINE Infolog~nCONSTANTS ',[]),mapseplist(prfunc(S),write_sep(b,S),L),
   format(S,'~nPROPERTIES~n',[]).

%%
% Write the footer to an open export file.
% @param Format the file format (clj, b)
% @param S the open file
end_file(clj,S) :- format(S,'}~n',[]).
end_file(b,S) :- format(S,' 1=1~nEND~n',[]).

%%
% Start writing a predicate to an open file.
% @param Format the file format (clj, b)
% @param S the open file
% @param P the predicate name (without arity)
start_pred(clj,S,P) :- format(S,':~w~n [',[P]).
start_pred(b,S,P) :- format(S,' ~w = ~n {',[P]), assert(first_tuple).

%%
% Finish writing a predicate to an open file.
% @param Format the file format (clj, b)
% @param S the open file
% @param P the predicate name (without arity)
end_pred(clj,S,_P) :- format(S,'~n ]~n',[]).
end_pred(b,S,_P) :- format(S,'~n } & ~n',[]), retractall(first_tuple).

%%
% Start writing a predicate solution tuple to an open file.
% @param Format the file format (clj, b)
% @param S the open file
start_tuple(clj,S) :- format(S,'[',[]).
start_tuple(b,S) :- (retract(first_tuple) -> format(S,'  (',[]) ; format(S,',  (',[])).

%%
% Finish writing a predicate solution tuple to an open file.
% @param Format the file format (clj, b)
% @param S the open file
end_tuple(clj,S) :- format(S,']~n',[]).
end_tuple(b,S) :- format(S,')~n',[]).

%%
% Write a value separator to an open file.
% @param Format the file format (clj, b)
% @param S the open file
write_sep(clj,_).
write_sep(b,S) :- format(S,', ',[]).

:- meta_predicate mapseplist(1,0,-), mapseplist2(-,1,0).

%%
% A variant of maplist/2 that intersperses the Pred calls with SepPred calls
% @param Pred the predicate to map on the list values
% @param SepPred the predicate to run in between
% @param List the list of parameters to Pred
mapseplist(_,_,[]).
mapseplist(P1,Sep,[H|T]) :- call(P1,H), mapseplist2(T,P1,Sep).

%%
% A variant of maplist/2 that runs SepPred before each Pred call
% (this is the tail processor for mapseplist/3)
% @param Pred the predicate to map on the list values
% @param SepPred the predicate to run first
% @param List the list of parameters to Pred
mapseplist2([],_,_).
mapseplist2([H|T],P1,Sep) :- call(Sep),call(P1,H), mapseplist2(T,P1,Sep).

%%
% Write an argument list to the open file.
% @param Args the argument list
% @param Format the file format (clj, b)
% @param S the open file
write_args(Args,Format,S) :- mapseplist(write_arg(S), write_sep(Format,S),Args).

%%
% Write a number or escaped argument to the open file.
% @param S the open file
% @param N the argument to write
write_arg(S,N) :- number(N),!, format(S,'~w ',[N]).
write_arg(S,N) :- escape_argument(N,EN),!,format(S,'"~w" ',[EN]).

% =========================================

% problems database

/**
 * Load and refresh the problem database at prolog-analyzer/problem_db.pl
 */
update_problem_db :- 
    update_problem_db('prolog-analyzer/problem_db.pl').
/**
 * Load and refresh the problem database at a given location.
 * @param File the file name. */
update_problem_db(File) :- 
   start_analysis_timer(T1),
   load_problem_db(File),
   stop_analysis_timer(T1),
   start_analysis_timer(T2),
   open(File,write,S), call_cleanup(gen_db_entries(S),close(S)),
   stop_analysis_timer(T2).

/**
 * Load the problem database from a given location.
 * @param File the file name. */
load_problem_db(File) :- 
   on_exception(error(existence_error(_,_),_),ensure_loaded(File), format('Problem DB does not yet exist: ~w~n',[File])),
   (problem_db_creation(S,D) -> format('Loaded problem_db ~w (Sha:~w,  ~w)~n',[File,S,D]) ; format('File empty: ~w~n',[File])).

:- dynamic problem_db_entry/8, problem_db_creation/2, problem_db_keep/6.
% problem_db_entry(HashOfIssue,Category,Type,ErrorInfo,Location,Sha,Date,active/reviewed)

/**
 * Avoid creating InfoLog warnings about the predicates listed in the infolog_predicate/3 facts.
 * @param Module the module name
 * @param F/N the predicate name and arity */
infolog_predicate(Module,F/N) :- infolog_predicate(F,N,Module), print(excl(F,N,Module)),nl.

/**
 * Avoid creating InfoLog warnings about the predicates listed in these facts.
 * @param Pred the predicate name
 * @param Arity the predicate arity
 * @param Module the module name */
infolog_predicate(problem_db_entry,8,user).
infolog_predicate(problem_db_creation,2,user).
infolog_predicate(problem_db_keep,6,user).

reviewed(Hash,Category,Type,ErrorInfo,Location) :-
    problem_db_entry(Hash,Category,Type,ErrorInfo,Location,_,_,reviewed).
     
:- use_module(library(system)).

/**
 * Update problem database; display new and removed problems.
 * @param S the open output file */
gen_db_entries(S) :-  print('Updating problem database and displaying new problems'),nl,
    format(S,'% INFOLOG DATABASE OF PROBLEMS~n',[]),
    git_revision(CurSha),
    datime(datime(Yr,Mon,Day,Hr,Min,Sec)),
    format('Sha : ~w, Date : ~w~n',[CurSha,datime(Yr,Mon,Day,Hr,Min,Sec)]),
    format(S, '% Updated: Sha : ~w, Date : ~w~n~n',[CurSha,datime(Yr,Mon,Day,Hr,Min,Sec)]),
    format(S,':- dynamic problem_db_entry/8, problem_db_creation/2.~n~n~n',[]),
    (problem_db_creation(CrS,CrD) -> portray_clause(S,problem_db_creation(CrS,CrD))
      ; portray_clause(S,problem_db_creation(CurSha,datime(Yr,Mon,Day,Hr,Min,Sec)))
    ),
    print('----'),nl, print('The following problems were added:'),nl,
    infolog_problem_hash(Category,Type,ErrorInfo,Location,Hash),
    (problem_db_entry(Hash, Category, Type, ErrorInfo,Location, OldSha,OldDatime,OldStatus)
     -> % error already exists
        Datime = OldDatime, NewSha = OldSha, Status = OldStatus, 
        assert_if_new(problem_db_keep(Hash,Category,Type,ErrorInfo,Location,unchanged))
     ; problem_db_entry(Hash, Category, Type, ErrorInfo,OldLocation, OldSha,OldDatime,OldStatus)
     -> % error has moved
        Datime = OldDatime, NewSha = OldSha, Status = moved,
        assert_if_new(problem_db_keep(Hash,Category,Type,ErrorInfo,OldLocation,moved)),
        format('Problem location has moved : ~w ',[Hash]), 
        print_location(OldLocation),print(' --> '), print_location(Location), nl
      ; Datime = datime(Yr,Mon,Day,Hr,Min,Sec), NewSha = CurSha, Status = active,
        display_problem(ErrorInfo,Location,Hash)
     ),
     portray_clause(S, problem_db_entry(Hash, Category, Type, ErrorInfo,Location, NewSha,Datime,Status) ),
    fail.
gen_db_entries(_) :- print('----'),nl, print('The following problems were removed:'),nl,
    problem_db_entry(Hash, Category, Type, ErrorInfo,Location, OldSha,OldDatime,_OldStatus),
    \+ problem_db_keep(Hash,Category,Type,ErrorInfo,Location,_),
    display_problem(ErrorInfo,Location,Hash), format('  ~w (~w)~n',[OldDatime,OldSha]),
    fail.
gen_db_entries(_) :- print('----'),nl.

% =========================================

% external calls from Tcl/Tk (Java still to do)
:- use_module(tcltk_calls, [tcltk_call/4]).
% tcltkc_call/4: tcltk_call(Name,Module,TclTkFile,Line)


/**
 * A variation of calling/6 which factors in also calls from external sources such as Tcl/Tk
 * @param M1 the caller module, or tcltk(Filename)
 * @param C1 the caller predicate/arity, or 'tcltk'
 * @param M2 the callee module
 * @param C2 the callee predicate/arity
 * @param L1 start line of the call
 * @param L2 end line of the call
 */
calling_with_ext(M1,C1,M2,C2,L1,L2) :- calling(M1,C1,M2,C2,L1,L2).
calling_with_ext(tcltkfile(File),tcltk,M2,Pred/Arity,Line,Line) :-
    tcltk_call(Pred,TkM2,File,Line),
    resolve_module_location_nondet(TkM2,Pred/Arity,M2).

%%
% Modules that are exported to external languages.
% TO DO: also add calls from Java ProB1/ProB2 to calling_with_ext;
% currently we hard-code prob2_interface as an externally visible module
% @param Module the module name
module_is_exported_to_external_language(prob2_interface).
module_is_exported_to_external_language(eclipse_interface). % for ProB1 for Rodin

%%
% Predicates which are/can be used by SICStus automatically; they are not dead code
% @param Module the module name
% @param Pred/Arity the predicate name and arity
is_used_by_sicstus(M,verify_attributes/3) :- depends_on(M,atts).
is_used_by_sicstus(_,foreign_resource/2).
is_used_by_sicstus(_,portray_message/2). % user can turn off messages this way
is_used_by_sicstus(_,runtime_entry/1). % for building binaries

% ==========================================

% a few indexing facts (variations of the above for better indexing lookups)

:- dynamic predicate_in/3. % predicate_in(Pred,Arity,Module) ; inverse of predicate/2 to quickly lookup module for given predicate name

/**
 * Look up the module for a given predicate/arity (calls predicate_in/3)
 * @param P/Arity the predicate and arity to look up.
 * @param Module the containing module */
predicate_in(P/Arity,Module) :- !, predicate_in(P,Arity,Module).
predicate_in(P,_) :- add_infolog_error(informat('Illegal predicate_in arg: ~w', [P])),fail.

/**
 * Look up the module for a given predicate and arity.
 * @param Pred the predicate name
 * @param Arity the predicate arity
 * @param Module the containing module */
predicate_in(A,B,C) :- nl,print('*** NOT COMPUTED  '),print(predicate_in(A,B,C)),nl,fail.

/**
 * Generate facts for predicate_in/3 using predicate/2. */
compute_indexing_facts :- retractall(predicate_in(_,_,_)),
    predicate(M,P),
    (P=Pred/Arity -> assert(predicate_in(Pred,Arity,M)) ; add_infolog_error(informat('Illegal pred: ~w', [P]))),
    fail.
compute_indexing_facts.

%%
% Check if a given predicate is imported explicitly via use_module(P,[PRED]) or implicitly via use_module(P)
% @param M the importer module
% @param Other the exporter module
% @param Pred the imported predicate
is_imported_from(M,Other,Pred) :-
   if(is_imported(M,Other,Pred),true,
      (is_exported(Other,Pred), depends_on(M,Other))).

% ==========================================
% DERIVED RULES to examine the CORE InfoLog database

/**
 * Check if a given predicate is exported by a module or a library
 * @param Module the exporting module
 * @param Pred the exported predicate */
is_exported_by_user_or_library(Module,Pred) :- is_exported(Module,Pred) ; is_exported_by_library(Module,Pred).

/**
 * True if the first module:predicate calls the second module:predicate somewhere.
 * @param M1:C1 the caller
 * @param M2:C2 the callee */
calling(M1:C1,M2:C2) :- calling(M1,C1,M2,C2,_,_).

/**
 * True if the first module:predicate calls the second module:predicate somewhere. Succeeds only once.
 * @param M1 the caller module
 * @param C1 the caller predicate
 * @param M2 the callee module
 * @param C2 the callee predicate */
calling(M1,C1,M2,C2) :- calling(M1,C1,M2,C2,L1,_), \+ (calling(M1,C1,M2,C2,L2,_),L2<L1). % try succeed just once

/**
 * True for any calls from M:P1 to M:P2 inside a module.
 * @param M:P1 the caller
 * @param M:P2 the callee */
calling_in_same_module(M:P,M:P2) :- calling(M,P,M,P2,_,_).

/**
 * True for any calls from M1:P1 to M2:P2 across module boundaries.
 * @param M1:P1 the caller
 * @param M2:P2 the callee */
calling_in_other_module(M:P,M2:P2) :- calling(M,P,M2,P2,_,_), M2 \= M.

%%
% Check if there is a dependency without a call that requires it.
% @param M1 the depending module
% @param M2 the dependency module
vacuous_module_dependency(M1,M2) :- depends_on(M1,M2),
   \+ calling(M1:_,M2:_).

%%
% Check various ways a module:call can be defined
% @param ToModule the callee module
% @param Call the callee predicate
is_defined(ToModule,Call) :- klaus(ToModule,Call,_,_).
is_defined(ToModule,Call) :- has_no_clauses(ToModule,Call).

%%
% Check if a module:call might be defined even though we cannot find any clauses
% (dynamic/foreign predicates, library modules, etc.)
% @param ToModule the callee module
% @param Call the callee predicate
has_no_clauses(ToModule,Call) :- is_dynamic(ToModule,Call).
has_no_clauses(ToModule,Call) :- is_chr_constraint(ToModule,Call).
has_no_clauses(ToModule,Call) :- is_attribute(ToModule,Call).
has_no_clauses(ToModule,Call) :- is_foreign(ToModule,Call).
has_no_clauses(ToModule,Call) :-  (is_library_module(ToModule) ; ToModule=user),
  (library_export_list_available(ToModule)
   -> (is_exported_by_library(ToModule,Call) -> true ; private_library_predicate(ToModule,Call))
    ; true). % list not available, assume it is defined
has_no_clauses(ToModule,Call) :- depends_on(ToModule,OtherModule),
  is_exported(OtherModule,Call). % Assume it is ok; an error would be generated in the other module ?! TO DO: we could recursively check if we can reach a definition

% ==========================================

% a few analysis utilities

/**
 * Counts the dynamic, public and exported predicates per module.
 * @param M the module
 * @param Dynamics the number of dynamic predicates
 * @param Public the number of public predicates
 * @param Exported the number of exported predicates
 * @justify setof required to find all predicates that fulfill is_dynamic/is_public/is_exported
 * @author Marvin Cohrs */
module_predicate_stats(M,Dynamics,Public,Exported) :-
    setof(X, X^is_dynamic(M,X), DynamicsSet),
    setof(X, X^is_public(M,X), PublicSet),
    setof(X, X^is_exported(M,X), ExportedSet),
    length(DynamicsSet, Dynamics),
    length(PublicSet, Public),
    length(ExportedSet, Exported).

/**
 * Counts the inbound and outbound calls per predicate.
 * @param Module the containing module
 * @param Pred the predicate and arity
 * @param InCount the number of incoming calls
 * @param OutCount the number of outgoing calls
 * @justify setof required to find all incoming/outgoing calls
 * @author Marvin Cohrs */
pred_incalls_outcalls(Module,Pred,InCount,OutCount) :-
    setof(M:P, M^P^SL^EL^calling(M,P,Module,Pred,SL,EL), InSet),
    setof(M:P, M^P^SL^EL^calling(Module,Pred,M,P,SL,EL), OutSet),
    length(InSet, InCount),
    length(OutSet, OutCount).

:- meta_predicate transitive(0,-).
%%
% Compute paths / transitive closure with at most one cycle and at least one step
transitive(M:Call,[A1,A2|Path]) :- binop(Call,P,A1,AFinal), %Call =.. [P,A1,AFinal],
   call(M:P,A1,A2),
   trans(M:P,A2,AFinal,[A1],Path).

%trans(P,A1,AFinal,History,Path) :- print(trans(P,A1,AFinal,History,Path)),nl,fail.
trans(_P,A,A,_,[]).
trans(P,A1,AFinal,History,[A2|Path]) :-
    nonmember(A1,History),
    call(P,A1,A2),
    trans(P,A2,AFinal,[A1|History],Path).

%%
% Compute dependency path between two modules using transitive/2.
% @param Module1 the depending module
% @param Module2 the transitive dependency
% @param Path the dependency path from Module1 to Module2
depends_path(Module1,Module2,Path) :- transitive(depends_on(Module1,Module2),Path).

%%
% Compute call path between two predicates using transitive/2.
% @param Call1 the caller predicate/arity
% @param Call2 the (transitive) callee predicate/arity
% @param Path the call path from Call1 to Call2
calls_path(Call1,Call2,Path) :- transitive(calling(Call1,Call2),Path).

%%
% Instantiate module dependency path with call witnesses, and print them to the console
% @param Path the dependency path
instantiate([_]).
instantiate([A,B|T]) :- calling(A:C1,B:C2),!,
    format('Module ~w -> ~w   [call: ~w -> ~w ]~n',[A,B,C1,C2]),
    instantiate([B|T]).
instantiate([A,B|_T]) :- format('*** Vacuous Module Dependency: ~w -> ~w~n',[A,B]),fail. % probably because of a a call in a :- declaration ?!?
% TO DO: probably also analyse :- directives

/**
 * Print X and add a line feed
 * @param X the text to print */
println(X) :- print(X),nl.

%%
% Find complex clauses and print them to the console
% @justify findall required for finding all clause_complexity facts
complexity :- findall(complexity(NestingLevel,Calls,M,P,SL,EL,Vs,Unif,EUnif),
                      (clause_complexity(M,P,NestingLevel,Calls,SL,EL,Vs,Unif,EUnif), (NestingLevel>3 ; Calls>15)),
                      List),
  sort(List,SortedList), maplist(println,SortedList),
  print(complexity('NestingLevel','Calls','Module','Pred','StartLine','EndLine','Variables','Unifications','ExplicitUnifications')),nl.

%%
% Print infolog problems of type "error" to the console
lint :- start_analysis_timer(T), print('Start checking'),nl,lint(error), stop_analysis_timer(T).

%%
% Print infolog problems of a given type to the console
% @param Type the problem type (error, warning, etc.)
lint(Type) :- lint(_,Type,_).

%%
% Print infolog problems of all types for a given module to the console
% @param M the module to inspect
lint_for_module(M) :- safe_defined_module(M), lint(_,_,M).

%%
% Print infolog problems of a given category and type to the console
% @param Category name of the problem rule
% @param Type error/warning/etc.
% @param Module the module to inspect
lint(Category,Type,Module) :-
     (nonvar(Module) -> dif(Location,unknown) ; true),
     infolog_problem_hash(Category,Type,ErrorInfo,Location,Hash),
     \+ reviewed(Hash,Category,Type,ErrorInfo,Location),
     (nonvar(Module) -> location_affects_module(Location,Module) ; true),
     display_problem(ErrorInfo,Location,Hash),
     fail.
lint(_,_,_) :- print('Done checking'),nl.

%%
% Print a given infolog problem to the console
% @param ErrorInfo ?
% @param Location the problem location
% @param Hash ?
display_problem(ErrorInfo,Location,Hash) :-
     format(' *** ',[]),
     print_information(ErrorInfo), print(' '),
     print_location(Location),
     format(' [[~w]]~n',[Hash]).

lint_for_pat(Pat) :- find_module(Pat,Module), lint_for_module(Module).

%%
% Find a module whose name contains the given pattern
% @param Pat the pattern
% @param Module the found module name
find_module(Pat,Module) :- defined_module(Module,_), atom_matches(Pat,Module).

%%
% Check if an atom's name is contained in another atom's name
% @param Pattern the needle
% @param Name the haystack
atom_matches(P,M) :- P=M,!.
atom_matches(Pattern,Name) :- atom_codes(Pattern,Codes),
   atom_codes(Name,NC), append(Codes,_,C),
   append(_,C,NC).

%%
% Export the found problems to a CSV file.
% @param File the file name
lint_to_csv_file(File) :- start_analysis_timer(T), format('Exporting to csv file: ~w~n',[File]),nl,
                          open(File,write,S), call_cleanup(lint_to_csv_stream(S),close(S)), stop_analysis_timer(T).

%%
% Export the found problems to the console in CSV format
lint_to_csv :- lint_to_csv_stream(user_output).

%%
% Export the found problems to an open stream in CSV format
% @param S the stream (open file or console)
lint_to_csv_stream(S) :-
     format(S,'~w,~w,~w,~w,~w,~w,~w,~w,~w~n',['Category','Type','Message','Module','Pred','File','L1','L2','Hash']),
     infolog_problem_flat(CatStr,Type,ErrStr,Module,Pred,File,L1,L2,Hash),
     format(S,'"~w",~w,"~w",~w,~w,"~w",~w,~w,~w~n',[CatStr,Type,ErrStr,Module,Pred,File,L1,L2,Hash]),
     fail.
lint_to_csv_stream(_).

% a flat view of infolog_problem, suitable for exporting (clojure, csv, tcltk):
infolog_problem_flat(CatStr,Type,ErrStr,Module,Pred,File,L1,L2,Hash) :-
     infolog_problem_hash(Category,Type,ErrorInfo,Location,Hash),
     decompose_location(Location,Module,Pred,L1,L2),
     (defined_module(Module,File) -> true ; File=unknown),
     information_to_atom(string(Category),CatStr),
     information_to_atom(ErrorInfo,ErrStr).

info :- info(_).
info(Category) :- infolog_info(Category,Info,Location),
     print_information(Info), print(' '),
     print_location(Location),nl,
     fail.
info(_).

% HERE WE DEFINE NEW PROBLEM RULES
% problem(CATEGORY, ErrorInformationTerm,  SourceLocationTerm)

/**
 * Generates problem entries.
 * @param Category the name of the rule
 * @param Type error/warning
 * @param ErrorInfo the problem description
 * @param Location the problem location */
infolog_problem(infolog_internal_error,error,P,Loc) :- infolog_internal_error(P,Loc).
infolog_problem(analysis_problem,error,string(P),Loc) :- problem(P,Loc).
infolog_problem(multiple_meta_predicates,error,informat('Multiple meta_predicate declarations for ~w:~w/~w (~w \\= ~w).',[Module,F,N,MetaArgList1,MetaArgList2]),module_loc(Module)) :-
        meta_user_pred(Head1,Module,MetaArgList1), functor(Head1,F,N), functor(Head2,F,N),
        meta_user_pred(Head2,Module,MetaArgList2), MetaArgList1 @> MetaArgList2.
infolog_problem(missing_import,warning,informat('Missing import ~w -> :- use_module(~w,~w).',[M1,M2,Calls]),module_loc(M1)) :-
        missing_imports(M1,M2,Calls).
infolog_problem(export_undefined,error,informat('Exporting undefined predicate ~w in module ~w.',[P,M]),module_loc(M)) :-
        is_exported(M,P), \+ is_defined(M,P).
infolog_problem(importing_undefined,error,informat('Importing undefined predicate ~w:~w.',[M,P]),module_loc(FromM)) :-
        is_imported(FromM,M,P), \+ is_defined(M,P).
infolog_problem(importing_private,error,informat('Importing private predicate ~w:~w.',[M,P]),module_loc(FromM)) :-
        is_imported(FromM,M,P), is_defined(M,P), \+ is_exported_by_user_or_library(M,P).
infolog_problem(export_multiple,info,informat('Predicate ~w exported by modules ~w and ~w.',[P,M1,M2]),module_loc(M1)) :-
        is_exported(M1,P), is_exported(M2,P), M2 @>M1.
infolog_problem(vacuous_modules,warning,informat('Vacuous module dependence ~w -> ~w',[M1,M2]),module_loc(M1)) :-
        vacuous_module_dependency(M1,M2).
infolog_problem(dead_modules,warning,informat('Dead module ~w',[M1]),module_loc(M1)) :-
        defined_module(M1,_), \+ (calling_with_ext(M2,_,M1,_,_,_), M1 \= M2).
infolog_problem(uncovered_calls,error,informat('Uncovered Call in module ~w :: ~w:~w',[FromModule,ToModule,Call]),
                                module_pred_lines(FromModule,FromQ,L1,L2)) :-
        uncovered_call(FromModule,FromQ,ToModule,Call,L1,L2),
        ToModule \= undefined_module. % these are already reported in analysis_problem above
infolog_problem(missing_meta_predicates,warning,informat('Missing ~w annotation for ~w:~w',[Msg,FromModule,Pred]),
                                        module_lines(FromModule,L1,L2)) :-
        uncovered_meta_call(FromModule,Pred,L1,L2,Msg).
infolog_problem(dead_code(private),warning,informat('Private predicate not used ~w',[P]),
                                        module_loc(M)) :- % TO DO: we could compute line info of first clause
        dca(all),dead_predicate(M,P).
infolog_problem(dead_code(exported),warning,informat('Exported predicate not used anywhere else: ~w',[P]), % could be used within module
                                        module_loc(M)) :- % TO DO: we could compute line info of first clause
        dca(cross),dead_predicate(M,P).
infolog_problem(useless_import,warning,informat('Imported predicate not used: ~w:~w',[M,P]),
                                        module_loc(From)) :- % TO DO: we could compute line info of first clause
        uia,useless_import(From,M,P).
infolog_problem(non_unifying_call,warning,informat('Call unifies with no clause: ~w:~w',[M,Call]),
                                        module_lines(FromModule,L1,L2)) :-
    non_unifying_call(M,Call,FromModule,L1,L2).
infolog_problem(undocumented_sideeffect,warning,informat('Predicate ~w has undocumented side effect ~w',[P,SE]),module_pred(M,P)) :-
    find_all_sideeffects,
    has_undocumented_sideeffect(M:P,SE).
infolog_problem(unexcused_metapred,warning,informat('Unjustified use of ~w in ~w',[MP,P]),module_pred(M,P)) :-
    has_unexcused_metapred_call(M:P,MP).

:- use_module(library(terms),[term_hash/2]).
%%
% A variant of infolog_problem/4, that also generates a hash over Category and ErrorInfo
% @param Category the rule name
% @param Type error/warning
% @param ErrorInfo the problem description
% @param Location the problem location
% @param Hash the hash over Category and ErrorInfo
infolog_problem_hash(Category,Type,ErrorInfo,Location,Hash) :-
     infolog_problem(Category,Type,ErrorInfo,Location),
     term_hash(infolog_problem(Category,ErrorInfo),Hash).

% HERE WE DEFINE INFOLOG INFOS

/**
 * Generates info entries.
 * @param Category the rule name
 * @param Desc the information description
 * @param Location the related location (if any) */
infolog_info(cycles,informat('Module Cycle ~w',[ModulePath]),unknown) :-
   defined_module(FromModule,_),
   (cycle(FromModule,ModulePath) -> true ; fail).
infolog_info(calls(FromModule,ToModule),informat('Call ~w:~w -> ~w:~w',[FromModule,C1,ToModule,C2]),
                                        module_lines(FromModule,L1,L2)) :-
   defined_module(FromModule,_),
   calling(FromModule,C1,ToModule,C2,L1,L2).

%%
% Is there a cycle in the module dependency graph?
% @param Module the start module
% @param ModulePath the dependency graph from Module to itself
cycle(Module,ModulePath) :-
   depends_path(Module,Module,ModulePath),
   instantiate(ModulePath).

% TO DO: provide more efficient way of computing this; maybe saturation BUP approach
cycle_ids(Module,Len,ModulePath) :- length(ModulePath,Len), cycle(Module,ModulePath).

%%
% Is there a calling cycle which leaves a module and enters it again?
% @param Module the start module
% @param Call the start predicate
% @param Path the cyclic call path
cross_module_cycle(Module,Call,[Module:Call|Path]) :-
    calling(Module,Call,TargetModule,TargetCall,_L1,_L2),
    TargetModule \= Module,
    calls_path(TargetModule:TargetCall,Module:Call,Path),
    instantiate(Path).

%%
% Print all calls in a module to the console (uses print_calls/3)
% @param FromModule the caller module to inspect.
print_calls(FromModule) :- print_calls(FromModule,_).

%%
% Print all calls from a module to another one (uses print_calls/3)
% @param FromModule the caller module
% @param ToModule the callee module
print_calls(FromModule,ToModule) :- print_calls(FromModule,_,ToModule).

%%
% Print all calls from a given module:predicate to given module
% @param FromModule the caller module
% @param C1 the caller predicate
% @param ToModule the callee module
print_calls(FromModule,C1,ToModule) :-
   safe_defined_module(FromModule),
   format('Calls from ~w to ~w~n===================~n',[FromModule,ToModule]),
   calling(FromModule,C1,ToModule,C2,L1,L2),
   format('Call ~w  ->  ~w : ~w   [lines: ~w - ~w]~n',[C1,ToModule,C2,L1,L2]),
   fail.
print_calls(_,_,_) :- format('===================~n',[]).

%%
% Print the required :- use_module declarations for a module; could be copied and pasted into the source. Shorthand for print_uses/1.
% @param Module the depending module
pu(Module) :- print_uses(Module).

%%
% Print the required :- use_module declarations for a module; could be copied and pasted into the source.
% @param FromModule the depending module
print_uses(FromModule) :- format('~n MODULE IMPORTS for ~w~n',[FromModule]),
   print_uses(FromModule,_), fail.
print_uses(FromModule) :- nl,
   format('~n MISSING IMPORTS for ~w~n',[FromModule]),
   (missing_imports(FromModule,ToModule,AllCalls),
    print_use_module(ToModule,AllCalls),fail
   ; format('~n---~n',[])).

%%
% Print the required :- use_module declaration such that FromModule includes ToModule; complain if this is not necessary.
% @param FromModule the depending module
% @param ToModule the dependency module
print_uses(FromModule,ToModule) :- safe_defined_module(FromModule),
   depends_on(FromModule,ToModule),
   (calling(FromModule,_,ToModule,_,_,_)
    ->  findall(C2,calling(FromModule,_C1,ToModule,C2,_L1,_L2),Imports),
        sort(Imports,SortedImports),
        print_use_module(ToModule,SortedImports)
    ;   format(' *** unnecessary use_module(~w).~n',[ToModule])).

%%
% Print the required :- use_module declaration such that the given predicates from Module are imported; complain if not all of them are exported.
% @param Module the module to be imported
% @param List list of the required predicates
print_use_module(Module,List) :-
    format(':- use_module(~w,~w).~n',[Module,List]),
    (is_library_module(Module) ->
       (library_export_list_available(Module) ->
         exclude(is_exported_by_library(Module),List,NotExported),
         (NotExported=[] -> true
           ; format('*** LIBRARY PREDICATES NOT EXPORTED in ~w: ~w~n',[Module,NotExported]))
         ; true)
     ; exclude(is_exported(Module),List,NotExported),
      (NotExported = [] -> true
        ; format('*** NEED TO BE EXPORTED in ~w: ~w~n',[Module,NotExported]))
     ).

%%
% Compute all missing imports for one module and target module in one go; can be used to backtrack over all missing imports
% @param FromModule the caller module
% @param ToModule the callee module
% @param AllCalls list of missing imports
% @justify findall required to find all missing_import/3 solutions
missing_imports(FromModule,ToModule,AllCalls) :-
     safe_defined_module(FromModule),
     findall(M:P,missing_import(FromModule,M,P),Missing),
     Missing \= [],
     findall(MM,member(MM:_,Missing),MMs), sort(MMs,ModulesToImport),
     member(ToModule,ModulesToImport),
     findall(P,member(ToModule:P,Missing),Ps), sort(Ps,AllCalls).

%%
% Find a missing import from one module to another
% @param FromModule the caller module
% @param ToModule the callee module
% @param Call the callee predicate
missing_import(FromModule,ToModule,Call) :-
    uncovered_call(FromModule,_,M,Call,_,_),
    \+ is_imported(FromModule,ToModule,Call), % if it is imported: generate error elsewhere
    resolve_module_location(M,Call,ToModule).

%%
% Try and locate source modules for calls with undefined modules.
% @param Module known module name or 'undefined_module'
% @param Pred predicate name
% @param Res resolved module name
resolve_module_location(undefined_module,PRED,Res) :- !,
  (is_exported_by_user_or_library(Module,PRED) -> Res=Module % there could be multiple solutions !
    ; predicate_in(PRED,Module) -> Res=Module % private predicate, there could be multiple solutions
    ; Res=undefined_module).
resolve_module_location(M,_,M).

%%
% A version of resolve_module_location which by backtracking generates all possible solutions
% @param Module known module name or 'undefined_module'
% @param Pred predicate name
% @param Res resolved module name
resolve_module_location_nondet(undefined_module,PRED,Res) :- !,
  if(is_exported_by_user_or_library(Module,PRED),Res=Module, % Note: if we find an export; we do not look among private predicates
     if(predicate_in(PRED,Module),Res=Module, % private predicate, there could be multiple solutions
         Res=undefined_module)
    ).
resolve_module_location_nondet(M,_,M).

%%
% Assert that the given module is defined; otherwise complain, but don't fail.
% @param A the module name
safe_defined_module(A) :- if(defined_module(A,_),true,format('*** Illegal module ~w~n',[A])).

% --------------------------------------------

% GLOBAL ANALYSES:
% these reqruire passes over all calls/clauses (indexing does not help); hence should be done
% for all modules/predicates in one go

% Dead Code Analysis (Global)
:- dynamic dead_predicate/2.

%%
% A wrapper for the dead code analysis in mode 'all', i.e. look at all not exported predicates whether they are used.
dca :- print('Looking for internal predicates which are not used'),nl,dca(all),print_dca(all).

%%
% A wrapper for the dead code analysis in mode 'cross', i.e. look at all exported predicates whether they are used by another module.
dcax :- print('Looking for exported predicates which are not used'),nl, dca(cross),print_dca(cross).

%%
% A simple dead code analysis; will not detect groups of dead code predicates which call each other.
% Warning: some predicates are called from Tcl/Tk, some from probcli only, some from ProB Tcl/Tk only.
% all: look at all not exported predicates whether they are used.
% cross: look at all exported predicats whether they are used by another module.
% @param Type 'all' or 'cross'
dca(Type) :- retractall(dead_predicate(_,_)),
       predicate(M,P),
       (Type=cross -> is_exported(M,P) ; \+ is_exported(M,P)),
       \+ is_public(M,P),
       \+ is_used_by_sicstus(M,P),
       \+ is_attribute(M,P), % TO DO: we could check attribute usage with put_atts/get_atts
       assert(dead_predicate(M,P)),fail.
dca(cross) :- calling_with_ext(M1,_,M,P,_,_), M1 \= M, % only look at cross_module calls
       retract(dead_predicate(M,P)),fail.
dca(all) :- calling_with_ext(_,_,M,P,_,_), % TO DO: check caller is not the predicate itself in case we remove recursive_call/0 generation
       retract(dead_predicate(M,P)),fail.
dca(cross) :- module_is_exported_to_external_language(M), retractall(dead_predicate(M,_)),fail.
dca(_).

%%
% Print the results of a dead code analysis. This requires that dca/1 has already
% been run.
% @param Type 'all' or 'cross' (see dca/1)
print_dca(Type) :- nl,print('dead predicates: '),print(Type),nl,
       dead_predicate(M,P), format(' ~w : ~w ~n',[M,P]),fail.
print_dca(_) :- nl.

:- dynamic useless_import/3.

%%
% Useless Import Analysis (Global)
uia :- retractall(useless_import(_,_,_)),
   is_imported(FromModule,M,P),
   \+ is_exported(FromModule,P), % we do not re-export the predicate
   assert(useless_import(FromModule,M,P)),fail.
uia :- 
   calling(FromModule,_,M,P,_,_), retract(useless_import(FromModule,M,P)),fail.
uia.

%%
% Run the useless import analysis and print its results to the console.
print_uia :- print('useless imports: '),nl,
       uia,
       useless_import(From,M,P), format(' In ~w import of ~w : ~w is useless~n',[From,M,P]),fail.
print_uia.

%%
% Find re-exports and print them to the console
print_reexports :- 
   is_imported(FromModule,M,P),
   is_exported(FromModule,P),
   format('Module ~w re-exports predicate ~w from module ~w~n',[FromModule,P,M]),fail.
print_reexports.

% ------------------

/**
 * Try and find calls where the predicate is not annotated with a meta_predicate
 * this indicates that the InfoLog meta-predicate call analysis could be imprecise
 */
uncovered_meta_call(FromModule,Pred,L1,L2,Msg) :-
   meta_call(FromModule,Pred,XX,NrAddedArgs,Head,L1,L2),
   (meta_pred_functor(Pred,FromModule,MetaList)
     -> get_required_meta_position(Head,XX,ArgNr),
        nonmember(meta_arg(ArgNr,NrAddedArgs),MetaList),
        gen_meta_pred_term(Pred,ArgNr,NrAddedArgs,Term),
        Msg = meta_predicate_for_position(ArgNr,Term) %arg(ArgNr,NrAddedArgs)
        %,print(missing_arg(ArgNr,NrAddedArgs,MetaList,Pred,Head)),nl
     ;  get_required_meta_position(Head,XX,ArgNr)
         -> gen_meta_pred_term(Pred,ArgNr,NrAddedArgs,Term),
            Msg = meta_predicate(Term) %no_annotation(ArgNr,NrAddedArgs)
     ;  Msg = meta_predicate).

gen_meta_pred_term(F/A, ArgNr,NrAddedArgs,Term) :-
   functor(Term,F,A),
   arg(ArgNr,Term,NrAddedArgs),
   Term =.. [_|Args],
   maplist(grnd,Args).
grnd(V) :- (V='-' -> true ; true).

get_required_meta_position(Head,XX,ArgNr) :-
     Head =.. [_|Args],
     nth1(ArgNr,Args,Arg),
     Arg==XX.

print_meta_calls(FromModule) :-
   defined_module(FromModule,_),
   format('Unresolved Meta Calls from ~w~n===================~n',[FromModule]),
   meta_call(FromModule,_,XX,NrAddedArgs,HEAD,L1,L2),
   (var(XX) -> XX = 'CALL' ; (XX=_:VV, var(VV)) -> VV='CALL' ; true),
   format('Call+~w:  ~w  ->  ~w   [lines: ~w - ~w of ~w]~n',[NrAddedArgs,HEAD,XX,L1,L2,FromModule]),
   fail.
print_meta_calls(_) :- format('===================~n',[]).

%%
% Try and find uncovered call, i.e. a call that is not defined or imported
% @param FromModule the caller module
% @param FromQ the caller predicate
% @param ToModule the callee module
% @param Call the callee predicate
% @param L1 start line of the call
% @param L2 end line of the call
uncovered_call(FromModule,FromQ,ToModule,Call,L1,L2) :- calling(FromModule,FromQ,ToModule,Call,L1,L2),
    (always_defined(Call) -> fail
     ; ToModule=built_in -> fail % we assume SICStus only assigns built_in if it exists
     ; is_defined(ToModule,Call)
     -> fail, % comment in to only detect calls without definition
        \+ check_imported(ToModule,Call,FromModule) % it is defined but not imported
     ;  true % it is not defined
     ).

%%
% Check if a given predicate is imported.
% @param Module the exporting module
% @param Call the predicate to check
% @param FromModule the importing module
check_imported(built_in,_Call,_) :- !. % assume built-in exists; TO DO: check ?
check_imported(M,_,M) :- !. % we see all predicates in our own module
check_imported(Module,Call,FromModule) :- is_imported(Module,Call,FromModule), % selectively imported
   !,
   (is_library_module(Module) -> true % ASSUME OK; TO DO: Check more rigourously
     ; is_exported(Module,Call) -> true
     ; format('Importing call which is not exported: ~w:~w~n',[Module,Call]),fail
    ).
check_imported(Module,Call,FromModule) :- depends_on(FromModule,Module), % imported, but not selectively imported
   (is_library_module(Module) -> true % ASSUME OK; TO DO: Check more rigourously
     ; is_exported(Module,Call)
    ).

%%
% Predicates that are always defined.
% @param Pred/Arity the predicate
always_defined(recursive_call/0).
always_defined(put_atts/2).
always_defined(get_atts/2).
always_defined(true/0).
always_defined(fail/0).
always_defined('~~'/1).  % ProB specific term expander; TO DO: get rid of this
always_defined(F/N) :- functor(Call,F,N), meta_pred(Call,built_in,_).



% -----------------------------------------
% Dot graph generation
:- use_module(infolog_dot_graph_generator).

dot_state_node(ID,none,Desc,box,none,green) :- defined_module(ID,_),
   \+ is_library_module(ID),
   ((depends_on_transitive(ID,_) ; depends_on_transitive(_,ID)) -> Desc=ID).
dot_state_trans(Module1,Label,Module2,Color,Style,PenWidth) :-
  dot_depends(Module1,Module2),
  (calling(Module1:_,Module2:P)
   -> Style=solid,
      findall(P2,calling(Module1:_,Module2:P2),AllP),
      length(AllP,NrCalls),
      sort(AllP,SortedAllP), length(SortedAllP,NrPreds),
      (depends_on_transitive(Module2,_) % we loop back to a starting module
        -> Label = 'CIRCULAR'(NrCalls,NrPreds,P), Color=red
        ; Label = uses(NrCalls,NrPreds,P),    Color=black),
      PenWidth is 1 + NrCalls // 10
    ; Style=dashed, Label = vacuous, Color=gray).
dot_depends(M1,M2) :- depends_on_transitive(Module1,Module2), \+ is_library_module(Module2),
    (depends_on(Module1,Module2),M1=Module1,M2=Module2 % the link itself
     ; % or another relevant link not included in the transitive closure from starting module
       % Note: the transitive closure only computes reachable nodes from the start node; not intermediate links
      Module1 \= Module2,
      depends_on(Module2,Module3),  \+ is_library_module(Module3),
      M1=Module2, M2=Module3, once(depends_on_transitive(_,Module3))
    ).

dot_gen_dep(Module) :- dot_gen_dep(Module,9999999).
dot_gen_dep(Module,MaxIter) :-
    defined_module(Module,_),
    retractall(depends_on_transitive(_,_)),
    transitive_closure(depends_on(Module,_),depends_on,depends_on_transitive,MaxIter),
    File = 'infolog.dot',
    format('Generating depends_on graph for ~w into file ~w~n',[Module,File]),
    il_gen_dot_graph(File,user,dot_state_node,dot_state_trans,none,none).

% -------------------------------------------

% analyze stored clauses and detect obvious errors

non_unifying_call(Module,Call,FromModule,SL,EL) :-
   %next_stored_clause_nr(N), format('Analyzing clauses in Module: ~w (total clauses ~w)~n',[Module,N]),
   stored_call(Module,Call,FromModule,Layout),
   \+ stored_clause(Module,Call,_,_),
   Module \= built_in,
   Module \= undefined_module, % already reported as error
   functor(Call,Pred,Arity),
   \+ has_no_clauses(Module,Pred/Arity),
   get_position(Layout,SL,EL),
   numbervars(Call,0,_).
   %format('No match for ~w in ~w:~w-~w~n',[Call,FromModule,SL,EL]).
%non_unifying_call(_,_,_,_,_).

% utility to obtain calls in the body of a clause
body_call(V,Call) :- var(V),!, Call=V.
body_call((A,B),Call) :- !, body_call(A,Call) ; body_call(B,Call).
body_call((A ; B),Call) :- !, body_call(A,Call) ; body_call(B,Call).
body_call((A -> B),Call) :- !, body_call(A,Call) ; body_call(B,Call).
body_call(\+(A),Call) :- !, body_call(A,Call).
body_call(when(_,A),Call) :- !, body_call(A,Call).
body_call(Body,Call) :- meta_pred(Body,_Module,List), member(meta_arg(Nr,Add),List),
   arg(Nr,Body,SubArg),
   body_call(SubArg,InnerCall),
   add_args(InnerCall,Add,Call).

% ==========================================
% a utility to compute the scc's
:- use_module(library(ugraphs),[vertices_edges_to_ugraph/3,reduce/2,vertices/2, edges/2]).

%%
% Translate a binary predicate into a ugraph, with an edge X-Y for each solution of Pred(X,Y).
% @param Pred a binary predicate
% @param UGraph the resulting graph
% @justify findall required to find all solutions for Pred
predicate_to_ugraph(Pred,UGraph) :-
   functor(TransCall,Pred,2), 
   arg(1,TransCall,X), arg(2,TransCall,Y),
   findall(X-Y,TransCall,Edges),
   vertices_edges_to_ugraph([], Edges, UGraph).

%%
% Create a ugraph for a binary predicate using predicate_to_ugraph/2 and
% reduce it to strongly connected components
% @param Pred a binary predicate
% @param SccGraph the resulting graph of strongly connected components
sccs(Pred,SccGraph) :-
   predicate_to_ugraph(Pred,UGraph),
   reduce(UGraph,SccGraph).

%%
% A variant of depends_on/2 that does not cover dependencies on libraries.
% @param X the depending module
% @param Y the dependency module
depends_on_non_library(X,Y) :- depends_on(X,Y), \+ is_library_module(Y).

%%
% Generate a graph of strongly connected components for module dependencies
% and write it to the file 'infolog.dot'.
sccs :- sccs(depends_on_non_library,Scc), print(Scc),nl,
    File = 'infolog.dot',
    format('Generating SCC graph for into file ~w~n',[File]),
    il_gen_dot_graph(File,user,dot_scc_node(Scc),dot_scc_trans(Scc),none,none).

dot_scc_node(SCC,ID,none,Desc,box,none,green) :-
    vertices(SCC,Vs), member(ID,Vs),
    length(ID,Len), ID = [First|_],
    (Len=1 -> Desc = First
     ; Desc = scc(First,Len,1)).
dot_scc_trans(SCC,S1,Label,S2,Color,Style,PenWidth) :- edges(SCC,Es), member(S1-S2,Es),
    PenWidth=1, Label = '',
    (length(S2,1) -> Color = black, Style = dashed
      ; Color = blue, Style=solid).

% ==========================================
% a utility to compute the transitive closure using a semi-naive algorithm:

:- dynamic new/2.
%%
% Compute transitive closure of binary predicate Pred and store result in dynamic binary predicate TransPred, using transitive_closure/4.
% @param Pred the binary predicate to test (only predicate name)
% @param TransPred the binary predicate to generate (only predicate name)
transitive_closure(Pred,TransPred) :- transitive_closure(Pred,TransPred,99999999).

%%
% Compute transitive closure of binary predicate Pred and store result in dynamic binary predicate TransPred, using transitive_closure/4.
% @param Pred the binary predicate to test (only predicate name)
% @param TransPred the binary predicate to generate (only predicate name)
% @param MaxIter the maximum number of iterations
transitive_closure(Pred,TransPred,MaxIter) :-
    functor(InitCall,Pred,2), transitive_closure(InitCall,Pred,TransPred,MaxIter).

%%
% Compute transitive closure of binary predicate Pred and store result in dynamic binary predicate TransPred.
% @param InitCall a call pattern Pred(_,_)
% @param Pred the binary predicate to test (only predicate name)
% @param TransPred the binary predicate to generate (only predicate name)
% @param MaxIter the maximum number of iterations
transitive_closure(InitCall,_Pred,TransPred,_MaxIter) :- retractall(new(_,_)),
    % copy facts matching InitCall:
    arg(1,InitCall,X), arg(2,InitCall,Y),
    call(InitCall), %print(init(InitCall)),nl,
    %print(i),
    assert(new(X,Y)),
    assert2(TransPred,X,Y),fail.
transitive_closure(_,Pred,TransPred,MaxIter) :- % start iteration
    nl,print('.'), flush_output(user_output),
    (MaxIter<1 -> nl
     ; transitive_closure_iterate(Pred,TransPred,MaxIter)
    ),
    count_trans_result(TransPred).

%%
% Count the solutions for the transitive closure TransPred and write them to the
% console.
% @param TransPred the binary predicate that was generated
% @justify findall required to find all solutions for TransPred(_,_)
count_trans_result(TransPred) :- binop(DerivedFact,TransPred,_,_),
     findall(DerivedFact,DerivedFact,L),
     length(L,Nr),
     format('Solutions in transitive closure of ~w : ~w~n',[TransPred,Nr]).

%%
% Generate new facts for the transitive predicate by adding more call edges
% to facts generated by the previous iterations.
% @param Pred the binary predicate to test
% @param TransPred the binary predicate to generate
% @param MaxIter the maximum number of iterations
transitive_closure_iterate(Pred,TransPred,_MaxIter) :-
     retract(new(X,Y)),
     call(Pred,Y,Z), % try and extend this new edge with all possible pairs from original relation
     binop(DerivedFact,TransPred,X,Z),
     \+(DerivedFact), % we have found a new fact
     %print(derived(DerivedFact)),nl,
     assert(new(X,Z)), assert(DerivedFact),
     fail.
transitive_closure_iterate(Pred,TransPred,MaxIter) :-
     (new(_,_) % we have added a new fact not yet processed
       ->  print('.'), flush_output(user_output),
           (MaxIter>0 -> M1 is MaxIter-1,
                         transitive_closure_iterate(Pred,TransPred,M1)
                      ;  print('maximum number of iterations reached'),nl)
        ;  print('Finished'),nl
     ).

%%
% Find all transitively inherited side effects.
% @sideeffect retract Removes facts for has_sideeffect/2
% @sideeffect assert Generates facts for has_sideeffect/2
% @similarto transitive_closure/2
% @author Marvin Cohrs
find_all_sideeffects :-
    retractall(has_sideeffect(_,_)),
    retractall(new(_,_)),
    % builtin's are marked as has_documented_sideeffect by java code 
    has_documented_sideeffect(Pred,SE,_),
    \+ no_need_to_document(Pred,SE),
    assert(has_sideeffect(Pred,SE)),
    %print(initial_has_sideeffect(Pred,SE)),nl,
    assert(new(Pred,SE)), fail.
find_all_sideeffects :-
    find_all_sideeffects_iterate(99999999).


%no_need_to_document(built_in:print/1, print).
no_need_to_document(_,print).
no_need_to_document(_,open).
no_need_to_document(_,close).
no_need_to_document(_,assert).
no_need_to_document(_,retract).

%has_documented_sideeffect(built_in:nl/0, print, 'Prints a line feed to the console').
%has_documented_sideeffect(built_in:format/2, print, 'Prints formatted text to the console').
%has_documented_sideeffect(built_in:format/3, print, 'Prints formatted text to a stream').
%has_documented_sideeffect(built_in:open/3, open, 'Opens a file').
%has_documented_sideeffect(built_in:close/1, close, 'Closes a file').
%has_documented_sideeffect(built_in:assert/1, assert, 'Adds a fact to the database').
%has_documented_sideeffect(built_in:asserta/1, assert, 'Adds a fact to the database').
%has_documented_sideeffect(built_in:assertz/1, assert, 'Adds a fact to the database').
%has_documented_sideeffect(built_in:retract/1, retract, 'Removes a fact from the database').
%has_documented_sideeffect(built_in:retractall/1, retract, 'Removes matching facts from the database').


%%
% Generate new facts for has_sideeffect/2 by adding more call edges
% to facts generated by the previous iterations.
% @param MaxIter the maximum number of iterations
% @similarto transitive_closure_iterate/3
% @author Marvin Cohrs
find_all_sideeffects_iterate(_MaxIter) :-
    retract(new(Pred,SE)),
    calling(P, Pred),
    \+ has_sideeffect(P, SE),
    assert(has_sideeffect(P, SE)),
    assert(new(P,SE)), fail.
find_all_sideeffects_iterate(MaxIter) :-
    (new(_,_) ->
         (MaxIter>0 -> M1 is MaxIter-1,
                       find_all_sideeffects_iterate(M1) ; true) ; true).

%%
% Has the given predicate got any undocumented side effect?
% @param M:P module and predicate
% @param SE the undocumented side effect
% @author Marvin Cohrs
has_undocumented_sideeffect(M:P, SE) :-
    has_sideeffect(M:P, SE),
    \+ has_documented_sideeffect(M:P, SE, _).

%%
% Finds and lists undocumented side effects.
% @sideeffect retract Removes facts for calling_transitive/2 and has_sideeffect/2
% @sideeffect assert Generates facts for calling_transitive/2 and has_sideeffect/2
% @sideeffect print Lists undocumented side effects
% @author Marvin Cohrs
print_undocumented_sideeffects :-
    find_all_sideeffects,
    (has_undocumented_sideeffect(M:Pred,SE),
     format('Undocumented side effect ~w in ~w~n',[SE,M:Pred]),fail ; true).

%%
% Is the given predicate a metapredicate that requires justification?
% Currently findall/3, findall/4, setof/3, bagof/3
% @param M:P/A the module, predicate name and arity
% @param PN predicate name as used in the @justify tags
% @author Marvin Cohrs
%demand_justification_for(built_in:findall/3, findall).
%demand_justification_for(built_in:findall/4, findall).
demand_justification_for(built_in:bagof/3, bagof).
demand_justification_for(built_in:setof/3, setof).

%%
% Does the given predicate call any metapredicate without a justification?
% @param Pred the predicate to test
% @param MetaPred the unexcused meta-predicate call
% @author Marvin Cohrs
has_unexcused_metapred_call(Pred, MetaPred) :-
    demand_justification_for(MPA, MetaPred),
    calling(Pred, MPA),
    \+ has_metapred_excuse(Pred, MetaPred, _).
    
%%
% Assert a binary predicate fact, if not already generated.
% @param Pred the predicate name
% @param X the predicate's first parameter
% @param Y the predicate's second parameter
assert2(Pred,X,Y) :- binop(Fact,Pred,X,Y), assert_if_new(Fact).
% above we compute  Init <| closure1(pred)  [In B terms]
% TO DO: write a version which only computes the reachable set [can be more efficient] : closure1(pred)[Init]

:- dynamic depends_on_transitive/2.

%%
% Generates a transitive variant of depends_on/2 and prints dependency loops.
compute_cycles :- retractall(depends_on_transitive(_,_)),
    start_analysis_timer(T1),
    transitive_closure(depends_on,depends_on_transitive),
    stop_analysis_timer(T1),
    (depends_on_transitive(A,A), format('Cyclic dependency: ~w~n',[A]),fail ; true).

:- dynamic calling_transitive/2.

%%
% Generates a transitive variant of calling/2 and prints call cycles starting
% at the given predicate.
% @param From the containing module
% @param Call the start predicate
compute_call_cycles(From,Call) :- retractall(calling_transitive(_,_)),
    start_analysis_timer(T1),
    transitive_closure(calling(From:Call,_),calling,calling_transitive),
    stop_analysis_timer(T1),
    (calling_transitive(A,B), format('Dependency: ~w -> ~w~n',[A,B]),fail ; true).

%%
% Compute the predicates within a module that M:P depends on;
% helps in refactoring (deciding what else would need to move or be imported if we move M:P to another module)
% @param M:P the caller predicate
% @param M:P2 the callee predicate
% @param StartingPreds a list of caller predicates to try
calling_in_same_module_from(M:P,M:P2,StartingPreds) :- calling_in_same_module(M:P,M:P2), member(P,StartingPreds).

%%
% Compute a sorted list of all predicates that are transitively called
% by one of the given start predicates in the same module.
% @param Module the module to inspect
% @param Calls a list of start predicates
% @param SList the (transitively) called predicates
% @justify findall required for finding all generated calling_transitive/2 facts
% @sideeffect retract retracting calling_transitive/2
compute_intra_module_dependence(Module,Calls,SList) :-
    retractall(calling_transitive(_,_)),
    start_analysis_timer(T1),
    transitive_closure(calling_in_same_module_from(Module:_,_,Calls),
                       calling_in_same_module,calling_transitive),
    stop_analysis_timer(T1),
    findall(P2,calling_transitive(_,_:P2),List),
    sort(List,SList).

%%
% Print a sorted list of all predicates that are transitively called
% by a given predicate (list) to the console.
% @param M the module to inspect
% @param C a start predicate or a list of start predicates
% @sideeffect print Writing the predicate list to the console.
pred_links(M,C) :- predicate(M,C), !, pred_links(M,[C]).
pred_links(M,List) :-
      compute_intra_module_dependence(M,List,SList), % If SList /= List we could add SList to List below ?
      format('~nCalls ~w : ~w depend on:~n  ~w~n',[M,List,SList]),
      format('External calls:~n  ',[]),printall(M2:C2,(member(C,List),calling_in_other_module(M:C,M2:C2))),nl,
      format('Called by in ~w:~n  ',[M]),printall(C2,(member(C,List),calling_in_same_module(M:C2,M:C))),nl.

%%
% Print a list of all solutions for the given predicate.
% @param Term result pattern
% @param Call the tested predicate
% @justify findall required for finding all solutions
% @sideeffect print Writing the solution list to the console
printall(Term,Call) :- findall(Term,Call,L), sort(L,SL), print(SL).

% ==========================================


% split module into call equivalence classes using REM's algorithm

:- use_module(library(rem)).

:- dynamic rem_id/3, id_rem/2, next_rem_id/1.
rem_id(M/P,ID) :- rem_id(M,P,ID).

%%
% Split module into call equivalence classes using REM's algorithm.
% @param Module the module to inspect
% @justify findall required for finding all predicates
% @sideeffect print prints the resulting equivalence classes
% @sideeffect retract removes previous rem_id/3, id_rem/2 and next_rem_id/1 facts
% @sideeffect assert add facts for next_rem_id/1, rem_id/3, id_rem/2
rem(Module) :-
   print(computing_equivalence_classes(Module)),nl,
   findall(P,predicate(Module,P),List),
   retractall(rem_id(_,_,_)),retractall(id_rem(_,_)), retractall(next_rem_id(_)),
   assert(next_rem_id(1)),
   maplist(gen_gem_id,List),
   length(List,Len),
   %print(rem_creat(Len)),nl,
   rem_create(Len,R),
   findall(same_class(P1,P2), calling_in_same_module(Module:P1,Module:P2), EqList),
   %print(fold),nl,
   il_foldl(rem_add,EqList,R,RRes),
   %print(eqclasses),nl,
   findall(Head/Pred, (rem_head(H,RRes,Head),id_rem(H,Pred)),L),
   sort(L,SL),
   format('Call components of ~w~n',[Module]),
   print_classes(SL,0,Module).

%%
% Print call equivalence classes to the console.
% @param List remaining predicates
% @param Prev previous predicate
% @param Module containing module
% @sideeffect print prints the equivalence classes
print_classes([],_,_).
print_classes([Head/Pred|T],Head,Module) :- !,
    print_pred(Pred,Module), print_classes(T,Head,Module).
print_classes([Head/Pred|T],_,Module) :- print('--------'),nl,
    print_pred(Pred,Module), print_classes(T,Head,Module).

%%
% Print predicate details to the console.
% @param P/N predicate name and arity
% @param M containing module
% @sideeffect print prints the predicate details
print_pred(P/N,M) :- %print(p(P,N,M)),nl,
    (is_exported(M,P/N) -> EXP='*exported' ; EXP=''),
    (is_imported(Other,M,P/N) -> IMP=imported_eg_in(Other) ; IMP=''),
    (is_imported_from(M,Other,P/N) -> IMPF = imported_from(M) ; IMPF = ''),
    (calling_with_ext(_,_,M,P/N,_,_) -> DEAD='' ; DEAD='DEAD'),
    (is_public(M,P/N) -> PUBLIC='*public' ; PUBLIC=''),
    format('  ~w/~w  ~w ~w ~w ~w ~w~n',[P,N,EXP,IMP,DEAD,IMPF,PUBLIC]).

rem_add(same_class(P1,P2),R,R2) :-
  ((P1 = (:-)/1 ; P2 = recursive_call/0) -> R2=R % ignore this call
    ; rem_id(P1,I1), rem_id(P2,I2),rem_add_link(I1,I2,R,R2)),!.
rem_add(SC,R,R) :- print(err(SC)),nl.

gen_gem_id(P/N) :- retract(next_rem_id(C)), C1 is C+1, assert(next_rem_id(C1)),
  %print(gen(C,P:N)),nl,
  assert(rem_id(P,N,C)), assert(id_rem(C,P/N)).

%%
% Run a left fold on a given list with a given /3 predicate (Elem,OldVal,NextVal),
% using il_foldl2/4.
% @param Pred the ternary predicate
% @param List the list to fold
% @param Start an initial value
% @param Result the result
il_foldl(Pred,List,Start,Result) :-
    il_foldl2(List,Pred,Start,Result).

%%
% Run a left fold on a given list with a given /3 predicate (Elem,OldVal,NextVal)
% @param List the list to fold
% @param Pred the ternary predicate
% @param OldValue the input value
% @param NewValue the result
il_foldl2([],_Pred,Value,Value).
il_foldl2([Elem|Rest],Pred,OldValue,NewValue) :-
    call(Pred,Elem,OldValue,Value),
    il_foldl2(Rest,Pred,Value,NewValue).

% ==========================================

%% Entry-point: analyze_clj("/path/to/prob/src/prob_tcltk.pl", "name of clojure output")

/**
 * This was probably meant for generating infolog.edn, but doesn't seem to
 * be used any more.
 * @deprecated in favour of analyze/2 + export_to_clj_file/1 */
analyze_clj(InputFile,OutputFile) :-
    analyze(InputFile),
    print(exporting(OutputFile)),nl,
    start_analysis_timer(T3),
    export_to_clj_file(OutputFile),
    stop_analysis_timer(T3).

%%
% This is the usual entry-point.
% @example analyze("/path/to/prob/src/prob_tcltk.pl", "name of meta_user_pred_cache")
% @param InputFiles a list of files to analyze
% @param CacheFile path to the meta-predicate cache
analyze(InputFiles,CacheFile) :-
    format('~nINFOLOG: Loading meta_predicate cache file ~w~n',[CacheFile]),
    ensure_loaded(CacheFile),
    catch(ensure_loaded('documentation.pl'),_,catch(ensure_loaded('prolog-analyzer/documentation.pl'),_,print('Documentation coverage data could not be loaded.'))),
    analyse_files(InputFiles),
    (meta_user_pred_cache_needs_updating -> write_meta_user_pred_cache(CacheFile) ; format('INFOLOG: meta_predicate cache up-to-date.~n',[])).

%%
% A simpler variant of analyze/2. Uses a default path for the cache.
% @param InputFiles a list of files to analyze
analyze(InputFiles) :- analyze(InputFiles,'meta_user_pred_cache.pl').

%%
% Analyzes a list of files.
% @caution it's analyse_files, not analyze_files. #typodanger
% @param InputFiles a list of files to analyze
analyse_files(InputFiles) :-
    start_analysis_timer(T0),
    print('INFOLOG: precompiling library_modules'),nl,
    precompile_library_modules,
    stop_analysis_timer(T0),
    format('INFOLOG: loading modules: ~w~n',[InputFiles]),
    start_analysis_timer(T1),
    use_source_modules(InputFiles),
    nl,
    stop_analysis_timer(T1),
    nl, print('INFOLOG: updating calls'), nl,
    start_analysis_timer(T2),
    update,
    compute_indexing_facts,
    stop_analysis_timer(T2),
    nl.

%%
% Load one ore more source modules.
% @param Files a module name or a list of module names
use_source_modules([]) :- !.
use_source_modules([H|T]) :- !, format('~nINFOLOG: loading module: ~w~n',[H]),
                                use_module(H),
                                format('~nINFOLOG: finished loading module: ~w~n',[H]),
                                use_source_modules(T).
use_source_modules(M) :- use_module(M).

%%
% Initialize a analysis timer
% @param Timer the resulting timer
start_analysis_timer(timer(R,T,W)) :- statistics(runtime,[R,_]),
   statistics(total_runtime,[T,_]),
   statistics(walltime,[W,_]).

%%
% Show the time passed since the initialization of the timer.
% @param Timer the started timer
% @sideeffect print prints the time difference
stop_analysis_timer(T) :- stop_analysis_timer(T,[runtime/RT,total_runtime/RTT,walltime/WT]),
                          format('% INFOLOG: Analysis Runtime: ~w ms (total: ~w ms, walltime: ~w ms)~n',[RT,RTT,WT]).

%%
% Compute the time difference since the initialization of the time.
% @param Timer the started timer
% @param Results the resulting time differences
stop_analysis_timer(timer(R,T,W),[runtime/RT,total_runtime/RTT,walltime/WT]) :-!,
   statistics(runtime,[RE,_]),
   statistics(total_runtime,[TE,_]),
   statistics(walltime,[WE,_]),
   RT is RE-R, RTT is TE-T, WT is WE-W.


% ==========================================

%  TERM EXPANDER PART

:- meta_predicate assert_if_new(0).
%%
% Assert a fact if it is not known yet.
assert_if_new(P) :- (P -> true ; assert(P)).

:- meta_predicate assert_if_new(0).
assert_if_not_covered(P) :- copy_term(P,CP), numbervars(CP,0,_End),
   (CP -> true ; assert(P)).

%%
% Make a list flat. Uses flatten1/3.
% @param List input list with nested lists
% @param FlatList flat result list
aflatten(List,FlatList) :- flatten1(List,[],FlatList).

%%
% Make a list flat, with an accumulator.
% @param List input list with nested lists
% @param Accum accumulator
% @param FlatList flat result list
flatten1([],L,L) :- !.
flatten1([H|T],Tail,List) :- !, flatten1(H,FlatList,List), flatten1(T,Tail,FlatList).
flatten1(NonList,Tail,[NonList|Tail]).

%%
% Predicates that may be used in DCGs without receiving 2 more arguments.
% @param Pred predicate name
% @param Arity predicate arity
dcg_specialcase('=',2).
dcg_specialcase('!',0).
dcg_specialcase(':',2).

bind_args(Args,VC,VCN) :-
    term_variables(Args,Variables),
    bind_args2(Variables,VC,VCN).
bind_args2([],X,X).
bind_args2([V|Vs],VC,VCN) :-
    number_codes(VC,CodesVC),
    append("v",CodesVC,Codes),
    atom_codes(V,Codes),
    VCNT is VC + 1,
    bind_args(Vs,VCNT,VCN).

%%
% Get the nth term of a layout; complains if is not reachable. Uses layout_sub_term/4
% @param Layout layout list
% @param Position index of the wanted term
% @param Result the result term
layout_sub_term(Layout,Position,Result) :- layout_sub_term(Layout,Position,Result,unknown).

%%
% Get the nth term of a layout; complains if is not reachable.
% @param Layout layout list
% @param Position index of the wanted term
% @param Result the result term
% @param Loc location of the caller
layout_sub_term([],N,[],Loc) :- !, format('~n*** Could not obtain layout information (position ~w) at ~w.~n',[N,Loc]).
layout_sub_term([H|T],N,Res,Loc) :- !,
    (N=<1 -> Res=H ; N1 is N-1, layout_sub_term(T,N1,Res,Loc)).
layout_sub_term(Term,N,Res,Loc) :-
    format('~n*** Virtual position: ~w~n',[layout_sub_term(Term,N,Res,Loc)]), % can happen when add_args adds new positions which did not exist
    Res=Term.

%%
% For a term-expander context compute a position term; if not possible: constructs unknown
% @param CallingPredicate the calling predicate
% @param Layout the layout term
% @param PositionTerm the resulting position term
get_position_term(CallingPredicate,Layout,PositionTerm) :-
   decompose_call(CallingPredicate,Module,CallingPred),
   get_position(Layout, StartLine, EndLine),!,
   PositionTerm = module_pred_lines(Module,CallingPred,StartLine,EndLine).
get_position_term(CallingPredicate,_,PositionTerm) :-
   decompose_call(CallingPredicate,Module,_),!,
   PositionTerm = module_loc(Module).
get_position_term(_,_,unknown).


get_position(Layout, StartLine, EndLine) :-
  get_position1(Layout, Start, End),
  (Start = [] -> StartLine = -1 ; StartLine = Start),
  (End = [] -> EndLine = -1 ; EndLine = End).

get_position1(Layout, StartLine, EndLine) :-
    aflatten(Layout,[StartLine|FlatLayout]),
    (FlatLayout = [] -> EndLine = StartLine ; last(FlatLayout,EndLine)).

%%
% Matches against different arities of call(...); returns arity and predicate
% @param Call call(...) term
% @param Arity arity of the called predicate
% @param Pred name of the called predicate
is_meta_call_n(call(C),0,C).
is_meta_call_n(call(C,_),1,C).
is_meta_call_n(call(C,_,_),2,C).
is_meta_call_n(call(C,_,_,_),3,C).
is_meta_call_n(call(C,_,_,_,_),4,C).
is_meta_call_n(call(C,_,_,_,_,_),5,C).
is_meta_call_n(call(C,_,_,_,_,_,_),6,C).

% assert that a meta-call occured and could not be resolved statically
assert_unresolved_meta_call(VariableCall,ExtraArgs,Layout,CallingPredicate,DCG,Info) :-
    decompose_call(CallingPredicate,CM,CP),
    member(head/ClauseHead,Info),
    get_position(Layout, StartLine, EndLine),
    !,
    (DCG = dcg -> ExtraArgs2 is ExtraArgs+2 ; ExtraArgs2=ExtraArgs),
    %format('**add meta_call: Head = ~w, ~n ~w~n',[ClauseHead,meta_call(CM,CP,VariableCall,ExtraArgs2,ClauseHead, StartLine, EndLine)]),
    assert_if_not_covered(meta_call(CM,CP,VariableCall,ExtraArgs2,ClauseHead, StartLine, EndLine)).
assert_unresolved_meta_call(VariableCall,ExtraArgs,Layout,CallingPredicate,DCG,Info) :-
    format('*** ERROR: ~w~n',[assert_unresolved_meta_call(VariableCall,ExtraArgs,Layout,CallingPredicate,DCG,Info)]),
    get_position(Layout, StartLine, EndLine), format('*** LINES: ~w-~w~n',[StartLine,EndLine]).

     % calling(cmodule,cname/carity, module,name/arity, startline, endline)
assert_call(CallingPredicate, Predicate, Layout, DCG) :-
    (assert_call2(DCG,CallingPredicate, Predicate, Layout) -> true
      ;  get_position_term(CallingPredicate,Layout,PositionTerm),
         add_infolog_error(informat('*** assert_call failed ~w',[assert_call(CallingPredicate, Predicate, Layout, DCG)]),PositionTerm)).
assert_call2(DCG,CallingPredicate, Predicate, Layout) :-
    get_position(Layout, StartLine, EndLine),
    decompose_call(Predicate,Module,Call),
    functor(Call, Name, SourceArity),
    adapt_arity(DCG,SourceArity,Arity),
    decompose_call(CallingPredicate,CM,CP),
    (provide_debug_for_module(CM) ->
       format('Call:         ~w ---> ~w~n',[CallingPredicate,Predicate]),
       format('calling fact: ~w:~w ---> ~w:~w/~w on lines ~w-~w~n',[CM,CP,Module,Name,Arity,StartLine,EndLine])
       ; true),
    assert_if_new(calling(CM,CP, Module,Name/Arity, StartLine, EndLine)).

%%
% Increase the arity by 2 if the clause is a DCG.
% @param DCG 'dcg' or 'no_dcg'
% @param SourceArity assumed arity as found in the code
% @param Arity the corrected arity
adapt_arity(no_dcg,Arity,R) :- !, R=Arity.
adapt_arity(dcg,SourceArity,Arity) :- !,Arity is SourceArity+2.
%adapt_arity(meta(N),SourceArity,Arity) :- !,Arity is SourceArity+N.
adapt_arity(DCG,Arity,R) :- add_infolog_error(informat('unknown DCG type: ~w',[DCG])), R=Arity.

%%
% Analyze a clause body using analyze_body/5; on failure print an error message and recover
% @param Body the clause body to process
% @param Layout layout term
% @param CallingPredicate ?
% @param DCG 'dcg' or 'no_dcg'
% @param Info what is this?
safe_analyze_body(X,Layout, CallingPredicate, DCG, Info) :-
   (analyze_body(X,Layout, CallingPredicate, DCG, Info) -> true
     ; get_position_term(CallingPredicate,Layout,PositionTerm),
       add_infolog_error(informat('analyze body failed: ~w',[analyze_body(X,Layout, CallingPredicate, DCG)]),PositionTerm)
       %,trace, analyze_body(X,Layout, CallingPredicate, DCG, Info)
    ).

%%
% Analyze a clause body; assert all found calls (TODO what else does this do?)
% @param Body the clause body to process
% @param Layout layout term
% @param CallingPredicate ?
% @param DCG 'dcg' or 'no_dcg'
% @param Info what is this?

analyze_body(':'(_,_,_,FIX_THIS_CLAUSE),Layout, CallingPredicate, dcg, _Info).

analyze_body(VAR,Layout, CallingPredicate, DCG, Info) :-
    % print(analyze_body(VAR,'   ',layout(Layout), calling(CallingPredicate),dcg(DCG))),nl,
    (var(VAR) ; VAR=_Module:VV, var(VV)), !,
    assert_unresolved_meta_call(VAR,0,Layout, CallingPredicate, DCG, Info),
    assert_call(CallingPredicate, built_in:call(VAR), Layout, DCG). % DO we need this ?

analyze_body(X,Layout, CallingPredicate, DCG, Info) :-
    is_meta_call_n(X,N,VAR),
    (var(VAR) ; VAR=_Module:VV, var(VV)),
    !,
    assert_unresolved_meta_call(VAR,N,Layout, CallingPredicate, DCG, Info).


analyze_body(X,Layout,CallingPredicate,dcg,_) :-
    functor(X,F,A),
   % print(sc(X,F,A)),
    dcg_specialcase(F,A), !,
    assert_call(CallingPredicate, built_in:F/A, Layout, no_dcg).

% { ... } prevents DCGs from adding additional arguments
analyze_body({X},Layout,CallingPredicate,_DCG, Info) :- !,
    layout_sub_term(Layout,2,LayoutX,body_of_calling_predicate(CallingPredicate,Info)),
    analyze_body(X,LayoutX,CallingPredicate,no_dcg, Info).

%analyze_body(~~X,Layout,CallingPredicate,DCG) :- !,
%    analyze_body(X,Layout,CallingPredicate,DCG).

analyze_body(get_atts(A,B),Layout,CallingPredicate,DCG, _Info) :- !,
    CallingPredicate = M:_/_,
    assert_call(CallingPredicate, M:get_atts(A,B), Layout, DCG).

analyze_body(put_atts(A,B),Layout,CallingPredicate,DCG, _Info) :- !,
    CallingPredicate = M:_/_,
    assert_call(CallingPredicate, M:put_atts(A,B), Layout, DCG).

analyze_body(X,_,_,dcg, _Info) :- is_list(X),!.

analyze_body(\+(X),Layout, CallingPredicate, DCG, Info) :-
    !,
    assert_call(CallingPredicate, built_in:not(X), Layout, DCG),
    layout_sub_term(Layout,2,LayoutX,negation_in_body(CallingPredicate,Info)),
    safe_analyze_body(X,LayoutX,CallingPredicate,DCG, Info).

analyze_body((A -> B ; C),Layout, CallingPredicate, DCG, Info) :-
    !,
    assert_call(CallingPredicate, built_in:'->'(_,_,_), Layout, DCG),
    layout_sub_term(Layout,2,LayoutAB,ifte_body(CallingPredicate,Info)),
    layout_sub_term(LayoutAB,2,LayoutA,ifte_body(CallingPredicate,Info)),
    layout_sub_term(LayoutAB,3,LayoutB,ifte_body(CallingPredicate,Info)),
    layout_sub_term(Layout,3,LayoutC,ifte_body(CallingPredicate,Info)),
    safe_analyze_body(A,LayoutA, CallingPredicate, DCG, Info),
    safe_analyze_body(B,LayoutB, CallingPredicate, DCG, Info),
    safe_analyze_body(C,LayoutC, CallingPredicate, DCG, Info).

analyze_body((A,B),Layout, CallingPredicate, DCG, Info) :-
  !,
  layout_sub_term(Layout,2,LayoutA,conj1_body(CallingPredicate,Info)),
  layout_sub_term(Layout,3,LayoutB,conj2_body(CallingPredicate,Info)),
  safe_analyze_body(A,LayoutA, CallingPredicate, DCG, Info),
  safe_analyze_body(B,LayoutB, CallingPredicate, DCG, Info).

analyze_body((A;B),Layout, CallingPredicate, DCG, Info) :-
  !,
  layout_sub_term(Layout,2,LayoutA,or1_body(CallingPredicate,Info)),
  layout_sub_term(Layout,3,LayoutB,or2_body(CallingPredicate,Info)),
  safe_analyze_body(A,LayoutA, CallingPredicate, DCG, Info),
  safe_analyze_body(B,LayoutB, CallingPredicate, DCG, Info).

analyze_body((A->B),Layout, CallingPredicate, DCG, Info) :-
  !,
  layout_sub_term(Layout,2,LayoutA,body(CallingPredicate,Info)),
  layout_sub_term(Layout,3,LayoutB,body(CallingPredicate,Info)),
  safe_analyze_body(A,LayoutA, CallingPredicate, DCG, Info),
  safe_analyze_body(B,LayoutB, CallingPredicate, DCG, Info).

analyze_body(OrigMETA, OrigLayout, CallingPredicate, DCG, Info) :-
   (DCG=no_dcg
    -> META = OrigMETA, Layout = OrigLayout
     ; add_args(OrigMETA,2,META),
       add_layout(OrigLayout,2,Layout)
   ),
   find_meta_pred(META,MODULE,List,CallingPredicate),
   % TO DO: check that MODULE is also imported ! (use depends_on(,MODULE))
   ((MODULE=built_in ; decompose_call(CallingPredicate,CallingModule,_), depends_on(CallingModule,MODULE))
     -> true
      ; %format('*** meta_predicate not (yet) imported : ~w (from ~w)~n',[META,MODULE]),
        true
    ),
   !,
   %format('~n~n Analyze META ~w ~w ~w (from ~w)~n',[META,MODULE,List, CallingPredicate]),
   assert_call(CallingPredicate, MODULE:META, Layout, no_dcg),
   maplist(analyze_sub_arg(META, MODULE, Layout, CallingPredicate, Info),List).

analyze_body(Module:Call,Layout, CallingPredicate, DCG, _Info) :-
  !,
  CallingPredicate = CModule:N/A,
  ((CModule=Module,functor(Call,N,A)) ->
    assert_call(CallingPredicate, Module:recursive_call, Layout, DCG) ;
    assert_call(CallingPredicate, Module:Call, Layout, DCG)).

analyze_body(Call,Layout, CallingPredicate, DCG, _Info) :-
  CallingPredicate = Module:N/A,
  store_call(DCG,Call,Layout,Module), % optionally store actual call, not just arity
  (functor(Call,N,A) ->
    assert_call(CallingPredicate, Module:recursive_call, Layout, DCG) ;
    assert_call(CallingPredicate, module_yet_unknown:Call, Layout, DCG)).

% first look for local predicates in same module:
find_meta_pred(META,From,MetaList,From:_) :-
    meta_pred(META,From,MetaList),
    !.
% now give priority to finding meta_predicates of those predicates that are imported:
find_meta_pred(META,MODULE,MetaList,From:_) :-
    meta_pred(META,MODULE,MetaList),
    functor(META,F,N),
    is_imported(From,MODULE,F/N),
    !,
    true.
% now look everywhere:
find_meta_pred(META,MODULE,MetaList,_) :- 
    meta_pred(META,MODULE,MetaList).

% --------------------------
% Manipulating cache of meta_predicate annotations

:- dynamic meta_user_pred/3, meta_user_pred_cache_needs_updating/0.
%:- include(meta_user_pred_cache). % cached version from previous run;  TO DO provide parameter
% write meta_user_pred facts
write_meta_user_pred_cache(F) :- format('Writing meta_prediate Cache file: ~w~n',[F]),
     open(F,write,S), call_cleanup(gen_user(S),close(S)).
gen_user(S) :-
     format(S,':- dynamic meta_user_pred/3.~n',[]),
     meta_user_pred(H,M,L), format(S,'~k.~n',[meta_user_pred(H,M,L)]), %portray_clause(meta_user_pred(H,M,L)),nl,
     fail.
gen_user(_).

:- use_module(meta_pred_generator,[translate_meta_predicate_pattern/3]).


add_meta_predicate(Module,Pattern) :-
   translate_meta_predicate_pattern(Pattern,Head,MetaArgList),
   (meta_user_pred(Head,Module,MetaArgList) -> true
     ; format('~nINFOLOG: Adding meta_user_pred(~w,~w,~w)~n',[Head,Module,MetaArgList]),
       assert(meta_user_pred(Head,Module,MetaArgList)),
       (meta_user_pred_cache_needs_updating -> true
        ; assert(meta_user_pred_cache_needs_updating),
          add_infolog_error(informat('meta_predicate cache is out-of-date (e.g, ~w:~w) and needs to be refreshed',[Module,Head]))
       )
    ).

% --------------------------

meta_pred_functor(F/N,Module,MetaList) :- functor(Skel,F,N), meta_pred(Skel,Module,MetaList).

:- use_module(meta_preds,[meta_library_pred/3]).
% a list of predefined meta_predicates:
% meta_pred(CallSkeleton, DefiningModule, ListOfMetaArgs)
meta_pred(Var,_Module,_MetaList) :- var(Var),!,fail.
meta_pred(Module:Call,Module,MetaList) :- !, meta_pred(Call,Module,MetaList).
meta_pred(Call,Module,MetaList) :- meta_built_in_pred(Call,Module,MetaList),!.
meta_pred(Call,Module,MetaList) :- meta_library_pred(Call,Module,MetaList),!.
meta_pred(Call,Module,MetaList) :- meta_user_pred(Call,Module,MetaList), \+ ignore_meta_user_pred(Call,Module).

% the following meta_predicate annotations are just used for convenience to add module prefixes automatically
% they do not call the argument
ignore_meta_user_pred(add_failed_call_error(_),error_manager).
ignore_meta_user_pred(add_internal_error(_,_),error_manager).
ignore_meta_user_pred(module_info(_,_),module_information).

% built ins which are *not* dealt with specially by DCG rules
% Note: extra argument is only added at top-level for meta_pred (different behaviour to DCG expansion):
%| ?- maplist((q;r),[a,b],R).
%! Existence error in user:(;)/4
%! procedure user:(;)/4 does not exist
%! goal:  user:;(q,r,a,_23255)

meta_built_in_pred(when(_,_),built_in,[meta_arg(1,0),meta_arg(2,0)]).
meta_built_in_pred(on_exception(_,_,_),built_in,[meta_arg(2,0),meta_arg(3,0)]).
meta_built_in_pred(catch(_,_,_),built_in,[meta_arg(1,0),meta_arg(3,0)]).
meta_built_in_pred(if(_,_,_),built_in,[meta_arg(1,0),meta_arg(2,0),meta_arg(3,0)]).
%meta_built_in_pred(( _ -> _), built_in,[meta_arg(1,0),meta_arg(2,0)]). % dealt with specially in DCG mode
meta_built_in_pred(bagof(_,_,_),built_in,[meta_arg(2,0)]).
meta_built_in_pred(findall(_,_,_),built_in,[meta_arg(2,0)]).
meta_built_in_pred(findall(_,_,_,_),built_in,[meta_arg(2,0)]).
meta_built_in_pred(setof(_,_,_),built_in,[meta_arg(2,0)]).
meta_built_in_pred(assert(_),built_in,[meta_arg(1,0)]). % TO DO: keep more info about which predicate asserted
meta_built_in_pred(asserta(_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(assertz(_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(retract(_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(retractall(_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(call_cleanup(_,_),built_in,[meta_arg(1,0),meta_arg(2,0)]).
meta_built_in_pred(call_residue_vars(_,_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(call(_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(call(_,_),built_in,[meta_arg(1,1)]).
meta_built_in_pred(call(_,_,_),built_in,[meta_arg(1,2)]).
meta_built_in_pred(call(_,_,_,_),built_in,[meta_arg(1,3)]).
meta_built_in_pred(call(_,_,_,_,_),built_in,[meta_arg(1,4)]).
meta_built_in_pred(once(_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(do(_,_),built_in,[meta_arg(2,0)]).
% TO DO: support setof/3, bagof/3 arguments ^
% We could add ;/2, \+/1, ...


analyze_sub_arg(DModule:META,MODULE, Layout, CallingPredicate, Info, meta_arg(Nr,ADD) ) :- !,
  (DModule=MODULE -> true /* user provided additional module prefix; peel it off */
    ; format('*** Module prefix mismatch: ~w:~w (expected ~w)~n',[DModule,META,MODULE])),
   % we need to peel off layout to get to META:
   layout_sub_term(Layout,3,LayoutM,analyze_sub_arg_meta(CallingPredicate,Info)),
   analyze_sub_arg(META, MODULE, LayoutM, CallingPredicate, Info, meta_arg(Nr,ADD) ).
analyze_sub_arg(META, _, Layout, CallingPredicate, Info, meta_arg(Nr,ADD) ) :- Nr1 is Nr+1,
  layout_sub_term(Layout,Nr1,LayoutA,analyze_sub_arg(CallingPredicate,Info)),
  arg(Nr,META,SubArg), %print(add_args(SubArg,Nr,SubArgADD)),nl,trace,
  add_args(SubArg,ADD,SubArgADD),
  add_layout(LayoutA,ADD,LayoutA2),
  %format(' Analyze Sub ~w -> ~w  [ ~w ] (from ~w)~n',[Nr,ADD,SubArgADD,CallingPredicate]),
  safe_analyze_body(SubArgADD,LayoutA2, CallingPredicate, no_dcg, Info).

add_args(Call,0,Res) :- !, Res=Call.
add_args(Var,_N,Res) :- var(Var),!, Res=_.% causes problems with layout info: is_meta_call_n(Res,N,Var).
add_args(M:Call,N,Res) :- !, Res = M:CR, add_args(Call,N,CR).
add_args(Call,N,Res) :- %print(add(Call,N)),nl,
  Call =.. FA,
  length(Xtra,N), append(FA,Xtra,NFA),
   Res =.. NFA.

add_layout(Layout,0,R) :- !, R=Layout.
add_layout([L1|T],Nr,[L1|Res]) :- !, append(T,L1N,Res),
   repeat_el(Nr,L1,L1N).
add_layout(OrigLayout,_,Layout) :- Layout = [OrigLayout,OrigLayout,OrigLayout]. % for N=2, does not seem to work
repeat_el(0,_,Res) :- !, Res=[].
repeat_el(N,L1,[L1|T]) :- N1 is N-1, repeat_el(N1,L1,T).


problem(X) :- problem(X,_).
mk_problem(P,Loc) :- assert(problem(P,Loc)).
mk_problem(P) :- assert(problem(P,unknown)).

%% analyzing Prolog Code

% exporting as binary fact
add_fact2(Fact, Module, OtherModule:NA) :- !,
   print(switching_module_context(Fact,Module,OtherModule)),nl,  % happens e.g., for :-multifile user:term_expansion/6.
   add_fact2(Fact,OtherModule,NA).
add_fact2(Fact, Module, Name/Arity) :-
    assert_if_new(predicate(Module,Name/Arity)),
    binop(X,Fact,Module,Name/Arity), %X =..[Fact, Module, Name/Arity],
    assert(X).
add_fact3(Fact, Arg1, Arg2, Name/Arity) :-
    ternop(X,Fact,Arg1,Arg2,Name/Arity), %X =..[Fact, Arg1, Arg2, Name/Arity],
    assert(X).

% TO DO: remove add_fact and replace by add_fact2 for performance and indexing
add_fact(Fact, Module, Name/Arity) :- !,
    Predicate = Module:Name/Arity,
    assert_if_new(predicate(Module,Name/Arity)),
    unop(X,Fact,Predicate), %X =..[Fact, Predicate],
    assert(X).

add_fact(Fact, Module, Term ) :-
    functor(Term,Name,Arity),
    Term =..[_Fun|Arguments],
    Predicate = Module:Name/Arity,
    assert_if_new(predicate(Module,Name/Arity)),
    binop(X,Fact,Predicate,Arguments), %X =..[Fact, Predicate, Arguments],
    assert(X).



fixity(fy, prefix, right,1).
fixity(fx, prefix, not,1).

fixity(yf, prefix, left,1).
fixity(xf, prefix, not,1).

fixity(xfx, infix, not,2).
fixity(xfy, infix, right,2).
fixity(yfx, infix, left,2).


dependency(Module, Name) :-
  x_unwrap_module(Name,UnwrappedName),
  (defined_module(UnwrappedName,_) -> true;
    ( atom_concat('sicstus/', UnwrappedName, _T),
      atom_concat(_T,'.pl',File),
      assert_if_new(defined_module(UnwrappedName,File)))),
  assert_if_new(depends_on(Module,UnwrappedName)).


x_unwrap_module(chrsrc(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(library(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(probsrc(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(kodkodsrc(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(probcspsrc(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(probpromela(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(bparser(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(plugins(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(abstract_domains(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(tclsrc(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(smt_solvers_interface(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(probporsrc(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(prozsrc(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(probltlsrc(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(probpgesrc(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(smtlib_solver(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(symbolic_model_checker(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(prob_rewrite_rules(X),Y) :- !, remove_opt_path(X,Y).
x_unwrap_module(extension(E),Y) :- !, remove_opt_path(E,Y).
x_unwrap_module(Path,X) :- atom(Path),
    atom_chars(Path,PathChars),
    ( append(Base,[.,p,l],PathChars),
      suffix(Base,XChars)  % module loaded with .pl ending
    ; suffix(PathChars,XChars)), % or without
    x_remove_path(XChars,CharsWithoutPath),
    atom_chars(X,CharsWithoutPath).
x_unwrap_module(X,Res) :- functor(X,Prefix,1),!, print(ignoring(Prefix)),nl,
    arg(1,X,Inner), x_unwrap_module(Inner,Res).
x_unwrap_module(X,X) :- !. % might even be unwrapped

remove_opt_path(P,Module) :- atom_chars(P,ExtensionPath),
    (x_remove_path(ExtensionPath,MY) -> atom_chars(Module,MY)
     ; Module=P).

x_remove_path(L,L2) :-
    reverse(L,LR),
    nth0(N,LR,'/',_), %key code of /
    sublist(LR, LR2, 0, N, _),
    reverse(LR2,L2).

% analyze clauses via Term Expansion
analyze_clause((:- module(Name, ListOfExported)), _Layout, Module, File) :-
    !,
    x_unwrap_module(File,UnwrappedName),
    (Name = UnwrappedName -> true; mk_problem(wrong_filename(Module,Name,File))),
    (defined_module(Name2,File) -> mk_problem(multiple_modules_in_file(File, Name, Name2)); true),
    retractall(defined_module(Name,_)),
    assert_if_new(defined_module(Name,File)),
    maplist(add_fact2(is_exported, Name),ListOfExported).

analyze_clause((:- use_module(UsedModule, ListOfImported)), _Layout,Module, _File) :- % IMPORTS
    !, dependency(Module,UsedModule),
    x_unwrap_module(UsedModule,UnwrappedUsedName),
    x_unwrap_module(Module,UnwrappedName),
    maplist(add_fact3(is_imported,UnwrappedName,UnwrappedUsedName),ListOfImported). % TO DO: add source location

analyze_clause((:- use_module(X)), _Layout, Module, _File) :-
    (is_list(X) -> maplist(dependency(Module),X); dependency(Module,X)).
analyze_clause((:- prob_use_module(X)), _Layout, Module, _File) :-
    (is_list(X) -> maplist(dependency(Module),X); dependency(Module,X)).

analyze_clause((:- dynamic(X)), _Layout,Module,_File) :-
       !,
       pairs_to_list(X,L),
       exclude(infolog_predicate(Module),L,L1),
       maplist(add_fact2(is_dynamic, Module),L1).

analyze_clause((:- public(X)), _Layout,Module,_File) :-
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_public, Module),L).

analyze_clause((:- meta_predicate(X)), _Layout,Module, _File) :-
    !,
    pairs_to_list(X,L),
    maplist(add_fact(is_meta, Module), L),
    maplist(add_meta_predicate(Module), L).

%blocking, operator declarations, volatile, multifile, 	mode

analyze_clause((:- mode(X)), _Layout, Module, _File) :-
    !,
    pairs_to_list(X,L),
    maplist(add_fact(declared_mode, Module), L).

analyze_clause((:- block(X)), _Layout, Module, _File) :-
    !,
    pairs_to_list(X,L),
    maplist(add_fact(is_blocking, Module), L).

analyze_clause((:- op(Priority,FixityTerm,Name)), _Layout,Module, _File) :-
  fixity(FixityTerm, Fixity, Associativity, Arity),
  assert_if_new(predicate(Module,Name/Arity)),
  assert_if_new( operator(Module:Name/Arity,Priority,Fixity,Associativity) ).

analyze_clause((:- volatile(X)), _Layout,Module, _File) :-
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_volatile, Module),L).

analyze_clause((:- chr_constraint(X)), _Layout,Module, _File) :-
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_chr_constraint, Module),L).

analyze_clause((:- attribute(X)), _Layout,Module, _File) :- depends_on(Module,atts),
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_attribute, Module),L).

analyze_clause((:- chr_option(_,_)), _Layout, _Module, _File). % just ignore for analysis

analyze_clause((:- multifile(X)), _Layout, Module, _File) :-
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_multifile, Module),L).

analyze_clause(':-'(Body), Layout, Module, File) :- %portray_clause(query(Body,Layout)),
    !,
    layout_sub_term(Layout,2,LayoutSub,query(Module,File)),
    safe_analyze_body(Body,LayoutSub,Module:':-'/1, no_dcg, [query]). % TO DO: check why this fails

analyze_clause((Head :- Body), Layout, Module, File) :-
    !, %portray_clause((Head :- Body)),
    functor(Head,Name,Arity),
    % nl,nl,print(layout_clause(Name/Arity,Head,Body)),nl,
    Predicate = Module:Name/Arity,
    layout_sub_term(Layout,2,LayoutHead,clause(Module,File)),
    assert_head(Predicate, LayoutHead),
    store_clause(no_dcg,Module,Head,Body,Layout),
    layout_sub_term(Layout,3,LayoutSub,clause(Module,File)),
    % (Name=force_non_empty -> trace ; true),
    safe_analyze_body(Body,LayoutSub, Predicate, no_dcg,[head/Head]),
    analyze_clause_complexity(Module,Name/Arity,Head,Body,Layout),
    lint_body(Module,Name,Arity,Head,Body,LayoutSub).

analyze_clause((Head --> Body), Layout, Module, File) :- %portray_clause((Head --> Body)),
    !,
    functor(Head,Name,WrongArity),
    Arity is WrongArity + 2,
    Predicate = Module:Name/Arity,
    layout_sub_term(Layout,2,LayoutHead,dcg(Module,File)),
    assert_head(Predicate, LayoutHead),
    store_clause(dcg,Module,Head,Body,Layout),
    layout_sub_term(Layout,3,LayoutSub,dcg(Module,File)),
    safe_analyze_body(Body,LayoutSub, Predicate, dcg, [head/Head]), % TO DO: add two args to Head ?
    analyze_clause_complexity(Module,Name/Arity,Head,Body,Layout),
    lint_body(Module,Name,Arity,Head,Body,LayoutSub).


analyze_clause(runtime_entry(_), _L, _M, _F) :- !.
analyze_clause(end_of_file, _L, _M, _F) :- !.

analyze_clause(Fact, _Layout, Module, _File) :-
    functor(Fact,Name,Arity),infolog_predicate(Name,Arity,Module),!.
analyze_clause(Fact, Layout, Module, _File) :- %portray_clause( Fact ),
    !,
    %nl,print(fact(Fact)),nl,
    functor(Fact,Name,Arity),
    Predicate = Module:Name/Arity,
    assert_head(Predicate, Layout),
    store_clause(no_dcg,Module,Fact,true,Layout).

%%
% Compute complexity measurements on a given clause: nesting level, calls,
% variable count, unifications, explicit unifications, Halstead vocabulary,
% and Halstead length.
% @param Module containing module
% @param Predicate Predicate name and arity
% @param Head the clause head
% @param Body the clause body
% @param Layout the layout term
% @sideeffect assert adds facts for clause_halstead/8 and clause_complexity/9
analyze_clause_complexity(Module,Predicate,_Head,Body,Layout) :-
   get_position(Layout,StartLine,EndLine),
   term_variables(Body, Vars),
   length(Vars, VarCount),
   Predicate = _Name/Arity,
   (body_complexity(Body,cacc(0,0,0,0,0,2,Arity,[],[this:Predicate,':-'/2]),cacc(_,NestingLevel,Calls,Unifications,ExplicitUnifications,OperatorOcc,OperandOcc,AtomSet,OperatorSet))
     -> length(AtomSet, AtomCount),
      length(OperatorSet, OperatorCount),
      OperandCount is AtomCount+VarCount,
%      format('Halstead complexity ~w, ~w, ~w, ~w~n',[OperatorOcc,OperandOcc,OperatorCount,OperandCount]),
      assert(clause_halstead(Module,Predicate,StartLine,EndLine,OperatorOcc,OperandOcc,OperatorCount,OperandCount)),
      assert(clause_complexity(Module,Predicate,NestingLevel,Calls,StartLine,EndLine,VarCount,Unifications,ExplicitUnifications))
   ;  format('*** Computing clause complexity failed: ~w (~w-~w)~n',[Module,StartLine,EndLine])).

%%
% DCG for accumulationg the complexity of a clause body (see analyze_clause_complexity/5)
% @param Body the clause body
% @param In complexity before
% @param Out complexity afterwards
body_complexity(V) --> {var(V)},!.
body_complexity(_:V) --> {var(V)},!.
body_complexity((A,B)) --> !,  enter_scope(0),
   body_complexity(A), body_complexity(B), exit_scope(0).
body_complexity((A;B)) --> !,  enter_scope(1),
   body_complexity(A), body_complexity(B), exit_scope(1).
body_complexity(if(A,B,C)) --> !,
   enter_scope(1),
   body_complexity(A), body_complexity(B), body_complexity(C),
   exit_scope(1),
   inc_operator_occ, ins_operator(if/3).
body_complexity((A -> BC)) -->  {nonvar(BC), BC=(B ; C)}, !,
   enter_scope(1),
   body_complexity(A), body_complexity(B), body_complexity(C),
   exit_scope(1),
   inc_operator_occ, ins_operator('->'/2).
% Should we treat (Case1 -> B ; Case2 -> C ; .... ) specially as a case-construct ?
body_complexity((A->B)) --> !, inc_operator_occ, ins_operator('->'/2),
   % do not count otherwise -> as nesting
   ({A==otherwise} -> body_complexity(B), ins_operator(otherwise/0)
   ; enter_scope(1),
     body_complexity(A), body_complexity(B),
     exit_scope(1)).
body_complexity(\+ A) --> !,
   enter_scope(1),
   body_complexity(A),
   exit_scope(1),
   inc_operator_occ, ins_operator('\\+'/1).
body_complexity((A=B)) --> !,
   add_eunif,
   term_halstead(A), term_halstead(B),
   inc_operator_occ, ins_operator('='/2).
body_complexity(Meta) --> {unary_meta_built_in(Meta,A)},!,
   enter_scope(1),
   body_complexity(A),
   inc_operator_occ, {functor(Meta,Pred,Arity)}, ins_operator(meta:Pred/Arity),
   exit_scope(1).
body_complexity(Meta) --> {binary_meta_built_in(Meta,A,B)},!,
   enter_scope(1),
   body_complexity(A), body_complexity(B),
   inc_operator_occ, {functor(Meta,Pred,Arity)}, ins_operator(meta:Pred/Arity),
   exit_scope(1).
body_complexity(Module:Call) --> !,
   add_call(Module:Call),
   {Call =.. [Pred|Args], length(Args,Arity)},
   inc_operator_occ,
   ins_operator(Module:Pred/Arity),
   scanlist(term_halstead,Args).
body_complexity(Call) --> !,
   add_call(Call),
   {Call =.. [Pred|Args], length(Args,Arity)},
   inc_operator_occ,
   ins_operator(this:Pred/Arity),
   scanlist(term_halstead,Args).                       

%%
% DCG for processing the complexity of a term.
% @param Term the term to analyze
% @param In complexity before
% @param Out complexity afterwards
term_halstead(V) --> {var(V)}, !, inc_operand_occ.
term_halstead(F) --> !,
    {F =.. [Functor|Args], length(Args,Arity)}, 
    inc_operand_occ,
    ins_atom(Functor/Arity),
    scanlist(term_halstead,Args).

%%
% Match a binary meta predicate and return its call terms
% @param Term the meta-predicate term
% @param Call1 the first contained call
% @param Call2 the second contained call
binary_meta_built_in(when(W,A),W,A).
binary_meta_built_in(call_cleanup(A,B),A,B).
binary_meta_built_in(on_exception(_,A,B),A,B).
binary_meta_built_in(catch(A,_,B),A,B).

%%
% Match a unary meta predicate and return its call term
% @param Term the first contained call
% @param Call1 the contained call
unary_meta_built_in(findall(_,A,_),A).
unary_meta_built_in(findall(_,A,_,_),A).
unary_meta_built_in(bagof(_,A,_),A).
unary_meta_built_in(setof(_,A,_),A).
unary_meta_built_in(once(A),A).
%unary_meta_built_in(call(A),A).

%%
% Increment the nesting level
% @param Inc how much to add
% @param In complexity before
% @param Out complexity afterwards
enter_scope(Inc,cacc(Level,Max,Calls,Unif,EUnif,Otors,Onds,AS,OS),cacc(L1,M1,Calls,Unif,EUnif,Otors,Onds,AS,OS)) :-
    L1 is Level+Inc, (L1>Max -> M1=L1 ; M1=Max).

%%
% Decrement the nesting level
% @param Dec how much to subtract
% @param In complexity before
% @param Out complexity afterwards
exit_scope(Dec,cacc(Level,Max,Calls,Unif,EUnif,Otors,Onds,AS,OS),cacc(L1,Max,Calls,Unif,EUnif,Otors,Onds,AS,OS)) :-
    L1 is Level-Dec.

%%
% Increment the call, variable and unification counters
% @param Term the call term
% @param In complexity before
% @param Out complexity afterwards
add_call(otherwise) --> !,[].
add_call(fail) --> !,[].
add_call(true) --> !,[].
add_call(Term,cacc(L,M,Calls,Unif,EUnif,Otors,Onds,AS,OS),cacc(L,M,C1,Unif1,EUnif,Otors,Onds,AS,OS)) :-
    C1 is Calls+1,
    term_variables(Term,Vs),
    length(Vs,VC),
    Unif1 is Unif+VC.

%%
% Increment the unification and explicit unification counters
% @param In complexity before
% @param Out complexity afterwards
% @author Marvin Cohrs
add_eunif(cacc(L,M,Calls,Unif,EUnif,Otors,Onds,AS,OS),cacc(L,M,Calls,Unif1,EUnif1,Otors,Onds,AS,OS)) :-
    Unif1 is Unif+1,
    EUnif1 is EUnif+1.

%%
% Increment the operator occurence counter.
% @param In complexity before
% @param Out complexity afterwards
% @author Marvin Cohrs
inc_operator_occ(cacc(L,M,Calls,Unif,EUnif,Otors,Onds,AS,OS),cacc(L,M,Calls,Unif,EUnif,Otors1,Onds,AS,OS)) :- Otors1 is Otors+1.

%%
% Insert a new operator into the operator set
% @param Op the operator
% @param In complexity before
% @param Out complexity afterwards
% @author Marvin Cohrs
ins_operator(Op,cacc(L,M,Calls,Unif,EUnif,Otors,Onds,AS,OS),cacc(L,M,Calls,Unif,EUnif,Otors,Onds,AS,OS1)) :- sets:add_element(Op,OS,OS1).

%%
% Increment the operand occurence counter.
% @param In complexity before
% @param Out complexity afterwards
% @author Marvin Cohrs
inc_operand_occ(cacc(L,M,Calls,Unif,EUnif,Otors,Onds,AS,OS),cacc(L,M,Calls,Unif,EUnif,Otors,Onds1,AS,OS)) :- Onds1 is Onds+1.

%%
% Insert a new atom into the atom set
% @param Atom the atom
% @param In complexity before
% @param Out complexity afterwards
% @author Marvin Cohrs
ins_atom(Atom,cacc(L,M,Calls,Unif,EUnif,Otors,Onds,AS,OS),cacc(L,M,Calls,Unif,EUnif,Otors,Onds,AS1,OS)) :- sets:add_element(Atom,AS,AS1).

%%
% detect some obvious anti-patterns, problems in a clause:
lint_body(Module,Name,Arity,_Head,Body,BodyLayout) :-
    (dangerous_cut(Body,BodyLayout,StartLine, EndLine)
     -> Loc = module_pred_lines(Module,Name/Arity,StartLine,EndLine),
        mk_problem(dangerous_cut(Name,Arity),Loc),
        fail).
lint_body(_,_,_,_,_,_).
    
% look for dangerous cuts  p(..) :- Test,!,   A ; B.
dangerous_cut(';'(A,_B),Layout,StartLine, EndLine) :-
   layout_sub_term(Layout,2,LayoutA,cut_or(A)),
   contains_cut(A,LayoutA,StartLine, EndLine).
contains_cut('!',Layout,StartLine, EndLine) :-
   get_position(Layout,StartLine,EndLine).
% we could check if ! is not the last call; something like (p,q,! ; r,s) is possibly oks
contains_cut((A,B),Layout,StartLine, EndLine) :- 
    (layout_sub_term(Layout,2,LayoutA,cut_and1(A)),
     contains_cut(A,LayoutA,StartLine, EndLine)
     ;
     layout_sub_term(Layout,3,LayoutB,cut_and2(B)),
     contains_cut(B,LayoutB,StartLine, EndLine)).



% analyze_foreign/5
% analyze foreign declarations
analyze_foreign(foreign(Name, PredSpec), Layout, Module, _File, (:- dynamic(Name/Arity))) :-
    !,
    functor(PredSpec,_,Arity),
    Predicate = Module:Name/Arity,
    assert_if_new(is_foreign(Module,Name/Arity)),
    assert_head(Predicate, Layout).

analyze_foreign(foreign(Name, _Lang, PredSpec), Layout, Module, _File, TermOut) :-
    analyze_foreign(foreign(Name, PredSpec), Layout, Module, _File, TermOut).

%%
% Add the head of a newly found clause or fact to our predicate/2 and klaus/4
% database.
% @param Predicate the predicate name
% @param Layout layout term
% @sideeffect assert adds facts to predicate/2 and klaus/4
assert_head(Predicate, Layout) :-
    decompose_call(Predicate,M,P),
    assert_if_new(predicate(M,P)),
    get_position(Layout, StartLine, EndLine),
    assert(klaus(M,P,  StartLine, EndLine)).

% optionally store clauses; will require much more time and memory
%:- dynamic next_stored_clause_nr/1, stored_clause/4.
store_clause(DCG,Module,Head,Body,Layout) :- retract(next_stored_clause_nr(N)),!,
    adapt_call_for_dcg(DCG,Head,AHead),
    assert(stored_clause(Module,AHead,Body,Layout)),
    N1 is N+1, assert(next_stored_clause_nr(N1)).
% TO DO: deal with DCG BODY
store_clause(_,_,_,_,_).

% optionally store actual calls; not just pred/arity
store_call(_,_,_,_) :- \+ next_stored_clause_nr(_),!. % storing disabled
store_call(no_dcg,Builtin,_,_) :- built_in_call(Builtin),!.
store_call(DCG,Module:Call,Layout,FromModule) :- !,
    adapt_call_for_dcg(DCG,Call,ACall),
    assert(stored_call(Module,ACall,FromModule,Layout)).
store_call(DCG,Call,Layout,FromModule) :- !,
    adapt_call_for_dcg(DCG,Call,ACall),
    assert(stored_call(module_yet_unknown,ACall,FromModule,Layout)).

adapt_call_for_dcg(no_dcg,C,R) :- !, R=C.
adapt_call_for_dcg(dcg,Call,NewCall) :- Call =.. [Pred|Args],
    append(Args,[_,_],NewArgs),
    NewCall =.. [Pred|NewArgs].

built_in_call(otherwise).
built_in_call(true).
built_in_call(fail).
built_in_call(append(_,_,_)).
built_in_call(!).
built_in_call(nl).
built_in_call(print(_)).
built_in_call(write(_)).
built_in_call(format(_,_)).
built_in_call(format(_,_,_)).

%%
% Peel off all : module constructors, using decompose_call2/4
% @param M:P predicate name with at least one module: prefix
% @param MR the extracted module name
% @param PR the extracted predicate name
% @sideeffect print prints an error message on failure
decompose_call(M:P,MR,PR) :- !,decompose_call2(P,M,MR,PR). 
decompose_call(P,module_yet_unknown,P) :- format('*** Unknown Module for ~w~n',[P]).

%%
% Peel off all : module constructors
% @param P predicate name, possible with more module: prefixes
% @param OuterModule the last stripped module: prefix
% @param MR the extracted module name
% @param PR the extracted predicate name
decompose_call2(P,OuterModule,MR,PR) :- var(P),!, (MR,PR)=(OuterModule:P).
decompose_call2(M:P,_OuterModule,MR,PR) :- !, decompose_call2(P,M,MR,PR).
decompose_call2(P,OuterModule,OuterModule,P).

%%
% Guess the module of a given predicate
% @param Name the predicate name
% @param Arity the predicate arity
% @param CallingModule the calling module
% @param Module the guessed module
% @param Loc location term
get_module(Name, Arity, CallingModule, built_in,_Loc) :-
   functor(Call, Name, Arity),
   predicate_property(CallingModule:Call,built_in),!.
get_module(Name, Arity, CallingModule, Module,_Loc) :-
   functor(Call, Name, Arity),
   predicate_property(CallingModule:Call,imported_from(Module)),!.
get_module(Name, Arity, CallingModule, CallingModule,_Loc) :-
   functor(Call, Name, Arity),
   (predicate_property(CallingModule:Call,interpreted); predicate_property(CallingModule:Call,compiled)),!.
get_module(chr_constraint, 1, CallingModule, CallingModule, _Loc) :- depends_on(CallingModule,chr),!.
get_module('#<=>', 2, _CallingModule, clpfd, _Loc) :- defined_module(clpfd,_),!.
get_module('#=', 2, _CallingModule, clpfd, _Loc) :- defined_module(clpfd,_),!.
get_module('#=>', 2, _CallingModule, clpfd, _Loc) :- defined_module(clpfd,_),!.
get_module(Name, Arity, CallingModule, Module, Loc) :-
   %functor(Call, Name, Arity), findall(Prop, predicate_property(CallingModule:Call,Prop), Ls), print(props(Name,Arity,CallingModule,Ls)),nl,nl,trace,
   mk_problem(could_not_infer_module(Name, Arity, CallingModule),Loc),
   Module = undefined_module.

%%
% Correct the module names in calling/6 and stored_call/4. No 'module_yet_unknown' shall remain.
update :-
    retract(calling(CallingModule,CallingPred, module_yet_unknown,Name/Arity, Start, End)),
    get_module(Name, Arity, CallingModule, Module, module_pred_lines(CallingModule,CallingPred,Start,End)),
    assert_if_new(predicate(Module,Name/Arity)),
    assert_if_new(calling(CallingModule,CallingPred, Module,Name/Arity, Start, End)),
    fail.
update :- retract(stored_call(module_yet_unknown,Call,FromModule,Layout)),
    functor(Call,Name,Arity),
    get_position(Layout,SL,EL),
    get_module(Name, Arity, FromModule, Module, module_lines(FromModule,SL,EL)),
    assert_if_new(stored_call(Module,Call,FromModule,Layout)),
    fail.
update.

:- multifile user:term_expansion/6.
:- dynamic seen_token/0.

:- prolog_flag(compiling,_,debugcode).
:- prolog_flag(source_info,_,on).
%:- prolog_flag(profiling,_,on).
:- prolog_flag(redefine_warnings,_,off).

safe_analyze_clause(Term,Layout,Module,File) :-
    if(analyze_clause(Term,Layout,Module,File),true,
       format('~n*** Analyzing Clause for ~w Failed: ~w~n~n',[Module,Term])).

%%
% Interpreter magic. Sicstus calls this predicate when encountering a clause.
% @param Term encountered term
% @param Layout layout information
% @param Tokens token list
% @param TermOut the result term
% @param LayoutOut the result layout
% @param Tokens the result token list
user:term_expansion(Term, Layout, Tokens, TermOut, Layout, [codeq | Tokens]) :-
    %print(d(Term, Tokens)),nl,
    %(Term = (atomic_eq_check(_,_,_) :- B) -> trace ; true),
    nonmember(codeq, Tokens), % do not expand if already expanded
    prolog_load_context(module, Module),
    prolog_load_context(file, File),
    (member(rm_debug_calls,Tokens) -> assert_if_new(seen_token); true),
    %(seen_token -> member(rm_debug_calls,Tokens) ; true), % I am not sure what the purpose of this is ? It certainly removes certain clauses from the analysis
  % print(expand(Module,Term)),nl,
    (  analyze_foreign(Term, Layout, Module, File, TermOut)
     ; safe_analyze_clause(Term, Layout, Module, File), 
      (Term=portray_message(informational,_) -> TermOut = '$ignored'(Term) ; TermOut = Term)),
    !.

%% KNOWN ISSUES

%%% Infolog cannot deal with calls of the form MODULE:(A,B)
%%% attribute usage is not checked
%%% tcltk_call does not store arity
%%% lint does not cache; lint_for_module computes almost all errors/warning
%%% meta_pred_cache needs to be cleaned from old entries, when predicates move from one module to another

%% TO DO:

%% DETECT unnecessary unification foldl(Module:Pred,List,Start,Result) :- foldl2(List,Module:Pred,Start,Result).

:- dynamic provide_debug_for_module/1.
%provide_debug_for_module(junit_tests).

/**
 * Display a nice welcome message, listing many beautiful predicates to try.
 * @sideeffect print prints a welcome message */ 
infolog_help :-
  nl,
  print('INFOLOG ENTRY: analyze("/path/to/prob/src/prob_tcltk.pl", "name of meta_user_pred_cache")'),nl,
  print('INFOLOG ENTRY: dot_gen_dep(Module), dot_gen_dep(Module,0)'),nl,
  print('INFOLOG ENTRY: rem(Module) - Equivalence classes of predicates in module'),nl,
  print('INFOLOG ENTRY: compute_cycles - compute cyclic module dependencies'),nl,
  print('INFOLOG ENTRY: cycle_ids(Module,Len,Path) - iterative deepening search for module cycles'),nl,
  print('INFOLOG ENTRY: compute_call_cycles(From,Call) - compute cyclic call dependencies'),nl,
  print('INFOLOG ENTRY: pred_links(Module,Predicate) - compute cyclic call dependencies'),nl,
  print('INFOLOG ENTRY: pu(Module) - print required use_module directives'),nl,
  print('INFOLOG ENTRY: print_uia - print useless use_module directives'),nl,
  print('INFOLOG ENTRY: print_reexports - print predicates re-exported'),nl,
  print('INFOLOG ENTRY: dca - dead code analysis'),nl,
  print('INFOLOG ENTRY: complexity - clause complexity analysis'),nl,
  print('INFOLOG ENTRY: lint - find problems'),nl,
  print('INFOLOG ENTRY: print_meta_calls(Module)'),nl,
  print('INFOLOG ENTRY: print_undocumented_sideeffects'), nl,
  print('INFOLOG ENTRY: sccs'),nl,
  nl,nl.
:- infolog_help.
