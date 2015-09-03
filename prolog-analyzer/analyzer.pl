

portray_message(informational, _).

:- use_module(escaper).
:- use_module(infolog_tools).
:- use_module(clojure_exporter,[export_to_clj_file/1, export/1]).
:- use_module(library_modules).

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(system)).
:- use_module(library(file_systems)).
:- use_module(library(codesio)).


% :- op(300, fy, ~~).

% THE DYNAMIC PREDICATES WHICH ARE PART OF THE ANALYSIS

:-  dynamic
    defined_module/2,% module(name,file)
    predicate/2,     % predicate(module,name/arity)
    is_dynamic/2,    % is_dynamic(module,name/arity)
    is_public/2,     % is_public(module,name/arity)
    is_volatile/2,   % is_volatile(module,name/arity)
    is_chr_constraint/2,   % is_chr_constraint(module,name/arity)
    is_attribute/2,  % is_attribute(module,name/arity)
    is_meta/2,       % is_meta(module:name/arity, meta_arguments)
    klaus/4,         % klaus(module,name/arity,  startline, endline)
    calling/6,       % calling(callingmodule,callingpredicate/callingarity, module,name/arity, startline, endline)
    meta_call/7,     % meta_call(callingmodule,callingpredicate/callingarity, VAR, ExtraArgs, ClauseHead, startline, endline)
    declared_mode/2, % declared_mode(module:name/arity, mode_arguments)
    is_exported/2,   % is_exported(module,name/arity)
    is_imported/3,   % is_imported(from_module,imported_module,imported_name/arity)
    depends_on/2,    % depends_on(local_module, imported_module) ;; local_module uses imported_module
    is_multifile/2,  % is_multifile(module,name/arity)
    is_blocking/2,   % is_blocking(module:name/arity, block_arguments)
    operator/4,      % operator(module:name/arity, priority, fixity, associativity)  ;; fixity : {prefix, infix, postfix}
    problem/2.       % problem(details,Loc)

% =========================================

% external calls from Tcl/Tk (Java still to do)
:- use_module(tcltk_calls, [tcltk_call/4]).
% tcltkc_call/4: tcltk_call(Name,Module,TclTkFile,Line)


% a variation of calling/6 which factors in also calls from external sources such as Tcl/Tk
calling_with_ext(M1,C1,M2,C2,L1,L2) :- calling(M1,C1,M2,C2,L1,L2).
calling_with_ext(tcltkfile(File),tcltk,M2,Pred/Arity,Line,Line) :-
    tcltk_call(Pred,TkM2,File,Line),
    resolve_module_location_nondet(TkM2,Pred/Arity,M2).
% TO DO: also add calls from Java ProB1/ProB2

% predicates which are/can be used by SICStus automatically; they are not dead code:
is_used_by_sicstus(M,verify_attributes/3) :- depends_on(M,atts).
is_used_by_sicstus(_,foreign_resource/2).
is_used_by_sicstus(_,portray_message/2). % user can turn off messages this way
is_used_by_sicstus(_,runtime_entry/1). % for building binaries

% ==========================================

% a few indexing facts (variations of the above for better indexing lookups)

:- dynamic predicate_in/3 % predicate_in(Pred,Arity,Module) ; inverse of predicate/2 to quickly lookup module for given predicate name
           .
predicate_in(P/Arity,Module) :- !, predicate_in(P,Arity,Module).
predicate_in(P,_) :- add_infolog_error(informat('Illegal predicate_in arg: ~w', [P])),fail.

predicate_in(A,B,C) :- nl,print('*** NOT COMPUTED  '),print(predicate_in(A,B,C)),nl,fail.

compute_indexing_facts :- retractall(predicate_in(_,_,_)),
    predicate(M,P),
    (P=Pred/Arity -> assert(predicate_in(Pred,Arity,M)) ; add_infolog_error(informat('Illegal pred: ~w', [P]))),
    fail.
compute_indexing_facts.

% check if exported explicitly via use_module(P,[PRED]) or implicitly via use_module(P)
is_imported_from(M,Other,Pred) :-
   if(is_imported(M,Other,Pred),true,
      (is_exported(Other,Pred), depends_on(M,Other))).

% ==========================================
% DERIVED RULES to examine the CORE InfoLog database

is_exported_by_user_or_library(Module,Pred) :- is_exported(Module,Pred) ; is_exported_by_library(Module,Pred).

calling(M1:C1,M2:C2) :- calling(M1,C1,M2,C2,_,_).
calling_in_same_module(M:P,M:P2) :- calling(M,P,M,P2,_,_).
calling_in_other_module(M:P,M2:P2) :- calling(M,P,M2,P2,_,_), M2 \= M.

% check if there is a dependency without a call that requires it:
vacuous_module_dependency(M1,M2) :- depends_on(M1,M2),
   \+ calling(M1:_,M2:_).

% check various way a module:call can be defined:
is_defined(ToModule,Call) :- klaus(ToModule,Call,_,_).
is_defined(ToModule,Call) :- is_dynamic(ToModule,Call).
is_defined(ToModule,Call) :- is_chr_constraint(ToModule,Call).
is_defined(ToModule,Call) :- is_attribute(ToModule,Call).
is_defined(ToModule,Call) :-  is_library_module(ToModule),
  (library_export_list_available(ToModule)
   -> (is_exported_by_library(ToModule,Call) -> true ; private_library_predicate(ToModule,Call))
    ; true). % list not available, assume it is defined
is_defined(ToModule,Call) :- depends_on(ToModule,OtherModule),
  is_exported(OtherModule,Call). % Assume it is ok; an error would be generated in the other module ?! TO DO: we could recursively check if we can reach a definition

% ==========================================

% a few analysis utilities

% compute paths / transitive closure with at most one cycle and at least one step
:- meta_predicate transitive(0,-).
transitive(M:Call,[A1,A2|Path]) :- binop(Call,P,A1,AFinal), %Call =.. [P,A1,AFinal],
   call(M:P,A1,A2),
   trans(M:P,A2,AFinal,[A1],Path).

%trans(P,A1,AFinal,History,Path) :- print(trans(P,A1,AFinal,History,Path)),nl,fail.
trans(_P,A,A,_,[]).
trans(P,A1,AFinal,History,[A2|Path]) :-
    nonmember(A1,History),
    call(P,A1,A2),
    trans(P,A2,AFinal,[A1|History],Path).

depends_path(Module1,Module2,Path) :- transitive(depends_on(Module1,Module2),Path).

calls_path(Call1,Call2,Path) :- transitive(calling(Call1,Call2),Path).

% instantiate module dependency path with call witnesses
instantiate([_]).
instantiate([A,B|T]) :- calling(A:C1,B:C2),!,
    format('Module ~w -> ~w   [call: ~w -> ~w ]~n',[A,B,C1,C2]),
    instantiate([B|T]).
instantiate([A,B|_T]) :- format('*** Vacuous Module Dependency: ~w -> ~w~n',[A,B]),fail. % probably because of a a call in a :- declaration ?!?
% TO DO: probably also analyse :- directives


lint :- start_analysis_timer(T), print('Start checking'),nl,lint(error), stop_analysis_timer(T).
lint(Type) :- lint(_,Type,_).
lint_for_module(M) :- safe_defined_module(M), lint(_,_,M).
lint(Category,Type,Module) :-
     (nonvar(Module) -> dif(Location,unknown) ; true),
     infolog_problem_hash(Category,Type,ErrorInfo,Location,Hash),
     \+ reviewed(Hash,Category,ErrorInfo,Location,_,_),
     (nonvar(Module) -> location_affects_module(Location,Module) ; true),
     format(' *** ',[]),
     print_information(ErrorInfo), print(' '),
     print_location(Location),
     format(' [[~w]]~n',[Hash]),
     fail.
lint(_,_,_) :- print('Done checking'),nl.

lint_for_pat(Pat) :- find_module(Pat,Module), lint_for_module(Module).
find_module(Pat,Module) :- defined_module(Module,_), atom_matches(Pat,Module).
% check if an atom's name is contained in another atom's name
atom_matches(P,M) :- P=M,!.
atom_matches(Pattern,Name) :- atom_codes(Pattern,Codes),
   atom_codes(Name,NC), append(Codes,_,C),
   append(_,C,NC).

lint_to_csv_file(File) :- start_analysis_timer(T), format('Exporting to csv file: ~w~n',[File]),nl,
    open(File,write,S), call_cleanup(lint_to_csv_stream(S),close(S)), stop_analysis_timer(T).
lint_to_csv :- lint_to_csv_stream(user_output).
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

info(Category) :- infolog_info(Category,Info,Location),
     print_information(Info), print(' '),
     print_location(Location),nl,
     fail.
info(_).

% HERE WE DEFINE NEW PROBLEM RULES
% problem(CATEGORY, ErrorInformationTerm,  SourceLocationTerm)

infolog_problem(infolog_internal_error,error,P,Loc) :- infolog_internal_error(P,Loc).
infolog_problem(analysis_problem,error,string(P),Loc) :- problem(P,Loc).
infolog_problem(multiple_meta_predicates,error,informat('Multiple meta_predicate declarations for ~w:~w/~w.',[Module,F,N]),module_loc(Module)) :-
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

:- use_module(library(terms),[term_hash/2]).
infolog_problem_hash(Category,Type,ErrorInfo,Location,Hash) :-
     infolog_problem(Category,Type,ErrorInfo,Location),
     term_hash(infolog_problem(Category,ErrorInfo),Hash).

:- dynamic reviewed/6.
% reviewed(HashOfIssue,Category,ErrorInfo,Location,User,Sha)
:- include(reviewed_db).
gen_reviewed(Hash,Category,ErrorInfo) :- infolog_problem_hash(Category,_Type,ErrorInfo,Location,Hash),
    git_revision(Sha),
    portray_clause( reviewed(Hash, Category,ErrorInfo,Location, _User, Sha) ),
    fail.
gen_reviewed(_,_,_).

% HERE WE DEFINE INFOLOG INFOS
infolog_info(cycles,informat('Module Cycle ~w',[ModulePath]),unknown) :-
   defined_module(FromModule,_),
   (cycle(FromModule,ModulePath) -> true ; fail).
infolog_info(calls(FromModule,ToModule),informat('Call ~w:~w -> ~w:~w',[FromModule,C1,ToModule,C2]),
                                        module_lines(FromModule,L1,L2)) :-
   defined_module(FromModule,_),
   calling(FromModule,C1,ToModule,C2,L1,L2).



% is there a cycle in the module dependency graph:
cycle(Module,ModulePath) :-
   depends_path(Module,Module,ModulePath),
   instantiate(ModulePath).
% TO DO: provide more efficient way of computing this; maybe saturation BUP approach

% is there a calling cycle which leaves a module and enters it again ?
cross_module_cycle(Module,Call,[Module:Call|Path]) :-
    calling(Module,Call,TargetModule,TargetCall,_L1,_L2),
    TargetModule \= Module,
    calls_path(TargetModule:TargetCall,Module:Call,Path),
    instantiate(Path).

print_calls(FromModule) :- print_calls(FromModule,_).
print_calls(FromModule,ToModule) :- print_calls(FromModule,_,ToModule).
print_calls(FromModule,C1,ToModule) :-
   safe_defined_module(FromModule),
   format('Calls from ~w to ~w~n===================~n',[FromModule,ToModule]),
   calling(FromModule,C1,ToModule,C2,L1,L2),
   format('Call ~w  ->  ~w : ~w   [lines: ~w - ~w]~n',[C1,ToModule,C2,L1,L2]),
   fail.
print_calls(_,_,_) :- format('===================~n',[]).

% print the required :- use_module declarations for a module; could be copied and pasted into the source
pu(Module) :- print_uses(Module).
print_uses(FromModule) :- format('~n MODULE IMPORTS for ~w~n',[FromModule]),
   print_uses(FromModule,_), fail.
print_uses(FromModule) :- nl,
   format('~n MISSING IMPORTS for ~w~n',[FromModule]),
   (missing_imports(FromModule,ToModule,AllCalls),
    print_use_module(ToModule,AllCalls),fail
     ; format('~n---~n',[])).
print_uses(FromModule,ToModule) :- safe_defined_module(FromModule),
   depends_on(FromModule,ToModule),
   (calling(FromModule,_,ToModule,_,_,_)
    ->  findall(C2,calling(FromModule,_C1,ToModule,C2,_L1,_L2),Imports),
        sort(Imports,SortedImports),
        print_use_module(ToModule,SortedImports)
    ;   format(' *** unnecessary use_module(~w).~n',[ToModule])).

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


% compute all missing imports for one module and target module in one go; can be used to backtrack over all missing imports
missing_imports(FromModule,ToModule,AllCalls) :-
     safe_defined_module(FromModule),
     findall(M:P,missing_import(FromModule,M,P),Missing),
     Missing \= [],
     findall(MM,member(MM:_,Missing),MMs), sort(MMs,ModulesToImport),
     member(ToModule,ModulesToImport),
     findall(P,member(ToModule:P,Missing),Ps), sort(Ps,AllCalls).

missing_import(FromModule,ToModule,Call) :- uncovered_call(FromModule,_,M,Call,_,_),
    \+ is_imported(FromModule,ToModule,Call), % if it is imported: generate error elsewhere
    resolve_module_location(M,Call,ToModule).

% try and locate source modules for calls with undefined modules:
resolve_module_location(undefined_module,PRED,Res) :- !,
  (is_exported_by_user_or_library(Module,PRED) -> Res=Module % there could be multiple solutions !
    ; predicate_in(PRED,Module) -> Res=Module % private predicate, there could be multiple solutions
    ; Res=undefined_module).
resolve_module_location(M,_,M).

% a version which by backtracking generates all possible solutions
resolve_module_location_nondet(undefined_module,PRED,Res) :- !,
  if(is_exported_by_user_or_library(Module,PRED),Res=Module, % Note: if we find an export; we do not look among private predicates
     if(predicate_in(PRED,Module),Res=Module, % private predicate, there could be multiple solutions
         Res=undefined_module)
    ).
resolve_module_location_nondet(M,_,M).

safe_defined_module(A) :- if(defined_module(A,_),true,format('*** Illegal module ~w~n',[A])).

% --------------------------------------------

% GLOBAL ANALYSES:
% these reqruire passes over all calls/clauses (indexing does not help); hence should be done
% for all modules/predicates in one go

% Dead Code Analysis (Global)
:- dynamic dead_predicate/2.
% a simple dead code analysis; will not detect groups of dead code predicates which call each other
% Warning: some predicates are called from Tcl/Tk, some from probcli only, some from ProB Tcl/Tk only
dca :- print('Looking for internal predicates which are not used'),nl,dca(all),print_dca(all).
dcax :- print('Looking for exported predicates which are not used'),nl, dca(cross),print_dca(cross).
% all: look at all not exported predicates whether they are used
% cross: look at all exported predicats whether they are used by another module
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
dca(_).
print_dca(Type) :- nl,print('dead predicates: '),print(Type),nl,
       dead_predicate(M,P), format(' ~w : ~w ~n',[M,P]),fail.
print_dca(_) :- nl.

:- dynamic useless_import/3.
% Useless Import Analysis (Global)
uia :- retractall(useless_import(_,_,_)),
   is_imported(FromModule,M,P),
   assert(useless_import(FromModule,M,P)),fail.
uia :- calling(FromModule,_,M,P,_,_), retract(useless_import(FromModule,M,P)),fail.
uia.
print_uia :- print('useless imports: '),nl,
       useless_import(From,M,P), format(' In ~w import of ~w : ~w is useless~n',[From,M,P]),fail.
print_uia.

% ------------------

% try and find calls where the predicate is not annotated with a meta_predicate
% this indicates that the InfoLog meta-predicate call analysis could be imprecise
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

% try and find uncovered call
uncovered_call(FromModule,FromQ,ToModule,Call,L1,L2) :- calling(FromModule,FromQ,ToModule,Call,L1,L2),
    (always_defined(Call) -> fail
     ; ToModule=built_in -> fail % we assume SICStus only assigns built_in if it exists
     ; is_defined(ToModule,Call)
     -> fail, % comment in to only detect calls without definition
        \+ check_imported(ToModule,Call,FromModule) % it is defined but not imported
     ;  true % it is not defined
     ).


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


% always defined
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
dot_state_trans(Module1,Label,Module2,Color,Style) :-
  dot_depends(Module1,Module2),
  (calling(Module1:_,Module2:P)
   -> Style=solid,
      (depends_on_transitive(Module2,_) % we loop back to a starting module
        -> Label = 'CIRCULAR'(P), Color=red
        ; Label = uses(P),    Color=black)
    ; Style=dashed, Label = vacuous, Color=gray).
dot_depends(M1,M2) :- depends_on_transitive(Module1,Module2), \+ is_library_module(Module2),
    (depends_on(Module1,Module2),M1=Module1,M2=Module2 % the link itself
     ; % or another relevant link not included in the transitive closure from starting module
      Module1 \= Module2,
      depends_on(Module2,Module3),  \+ is_library_module(Module3),
      M1=Module2, M2=Module3, once(depends_on_transitive(_,Module3))
    ).

dot_gen_dep(Module) :-
    defined_module(Module,_),
    transitive_closure(depends_on(Module,_),depends_on,depends_on_transitive),
    il_gen_dot_graph('infolog.dot',user,dot_state_node,dot_state_trans,none,none).

% -------------------------------------------

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
% a utility to compute the transitive closure using a semi-naive algorithm:

:- dynamic new/2.
% compute transitive clousre of binary predicate Pred and store result in binary predicate TransPred
transitive_closure(Pred,TransPred) :-
    functor(InitCall,Pred,2), transitive_closure(InitCall,Pred,TransPred).

transitive_closure(InitCall,_Pred,TransPred) :- retractall(new(_,_)),
    % copy facts matching InitCall:
    arg(1,InitCall,X), arg(2,InitCall,Y),
    call(InitCall), %print(init(InitCall)),nl,
    assert(new(X,Y)),
    assert2(TransPred,X,Y),fail.
transitive_closure(_,Pred,TransPred) :- % start iteration
    print('.'), flush_output(user_output),
    transitive_closure_iterate(Pred,TransPred).

transitive_closure_iterate(Pred,TransPred) :-
     retract(new(X,Y)),
     call(Pred,Y,Z), % try and extend this new edge with all possible pairs from original relation
     binop(DerivedFact,TransPred,X,Z),
     \+(DerivedFact), % we have found a new fact
     %print(derived(DerivedFact)),nl,
     assert(new(X,Z)), assert(DerivedFact),
     fail.
transitive_closure_iterate(Pred,TransPred) :-
     (new(_,_) % we have added a new fact not yet processed
       ->  print('.'), flush_output(user_output),
           transitive_closure_iterate(Pred,TransPred)
        ;  print('Finished'),nl).
assert2(Pred,X,Y) :- binop(Fact,Pred,X,Y), assert_if_new(Fact).
% above we compute  Init <| closure1(pred)  [In B terms]
% TO DO: write a version which only computes the reachable set [can be more efficient] : closure1(pred)[Init]

:- dynamic depends_on_transitive/2.
compute_cycles :- retractall(depends_on_transitive(_,_)),
    start_analysis_timer(T1),
    transitive_closure(depends_on,depends_on_transitive),
    stop_analysis_timer(T1).

:- dynamic calling_transitive/2.
compute_call_cycles(From,Call) :- retractall(calling_transitive(_,_)),
    start_analysis_timer(T1),
    transitive_closure(calling(From:Call,_),calling,calling_transitive),
    stop_analysis_timer(T1).

% compute the predicates within a module that M:P depends on
% helps in refactoring (deciding what else would need to move or be imported if we move M:P to another module)

calling_in_same_module_from(M:P,M:P2,StartingPreds) :- calling_in_same_module(M:P,M:P2), member(P,StartingPreds).

compute_intra_module_dependence(Module,Calls,SList) :-
    retractall(calling_transitive(_,_)),
    start_analysis_timer(T1),
    transitive_closure(calling_in_same_module_from(Module:_,_,Calls),calling_in_same_module,calling_transitive),
    stop_analysis_timer(T1),
    findall(P2,calling_transitive(_,_:P2),List),
    sort(List,SList).


pred_links(M,C) :- predicate(M,C), !, pred_links(M,[C]).
pred_links(M,List) :-
      compute_intra_module_dependence(M,List,SList), % If SList /= List we could add SList to List below ?
      format('~nCalls ~w : ~w depend on:~n  ~w~n',[M,List,SList]),
      format('External calls:~n  ',[]),printall(M2:C2,(member(C,List),calling_in_other_module(M:C,M2:C2))),nl,
      format('Called by in ~w:~n  ',[M]),printall(C2,(member(C,List),calling_in_same_module(M:C2,M:C))),nl.

printall(Term,Call) :- findall(Term,Call,L), sort(L,SL), print(SL).

% ==========================================


% split module into call equivalence classes using REM's algorithm

:- use_module(library(rem)).

:- dynamic rem_id/3, id_rem/2, next_rem_id/1.
rem_id(M/P,ID) :- rem_id(M,P,ID).
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

print_classes([],_,_).
print_classes([Head/Pred|T],Head,Module) :- !,
    print_pred(Pred,Module), print_classes(T,Head,Module).
print_classes([Head/Pred|T],_,Module) :- print('--------'),nl,
    print_pred(Pred,Module), print_classes(T,Head,Module).

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

il_foldl(Pred,List,Start,Result) :-
    il_foldl2(List,Pred,Start,Result).
il_foldl2([],_Pred,Value,Value).
il_foldl2([Elem|Rest],Pred,OldValue,NewValue) :-
    call(Pred,Elem,OldValue,Value),
    il_foldl2(Rest,Pred,Value,NewValue).

% ==========================================

%% Entry-point: analyze("/path/to/prob/src/prob_tcltk.pl", "name of clojure output")

analyze_clj(InputFile,OutputFile) :-
    analyze(InputFile),
    print(exporting(OutputFile)),nl,
    start_analysis_timer(T3),
    export_to_clj_file(OutputFile),
    stop_analysis_timer(T3).

analyze(InputFiles,CacheFile) :-
    format('Loading meta_predicate cache file ~w~n',[CacheFile]),
    ensure_loaded(CacheFile),
    analyse_files(InputFiles),
    (meta_user_pred_cache_needs_updating -> write_meta_user_pred_cache(CacheFile) ; format('meta_predicate cache up-to-date.~n',[])).
analyze(InputFiles) :- analyze(InputFiles,'meta_user_pred_cache.pl').

analyse_files(InputFiles) :-
    start_analysis_timer(T0),
    print('precompiling'),nl,
    precompile_library_modules,
    stop_analysis_timer(T0),
    print('loading modules'),nl,
    start_analysis_timer(T1),
    use_source_modules(InputFiles),
    nl,
    stop_analysis_timer(T1),
    nl, print('updating calls'), nl,
    start_analysis_timer(T2),
    update,
    compute_indexing_facts,
    stop_analysis_timer(T2),
    nl.

% works with single file or multiple files:
use_source_modules([]) :- !.
use_source_modules([H|T]) :- !, use_module(H), use_source_modules(T).
use_source_modules(M) :- use_module(M).


start_analysis_timer(timer(R,T,W)) :- statistics(runtime,[R,_]),
   statistics(total_runtime,[T,_]),
   statistics(walltime,[W,_]).
stop_analysis_timer(T) :- stop_analysis_timer(T,[runtime/RT,total_runtime/RTT,walltime/WT]),
   format('% Analysis Runtime: ~w ms (total: ~w ms, walltime: ~w ms)~n',[RT,RTT,WT]).
stop_analysis_timer(timer(R,T,W),[runtime/RT,total_runtime/RTT,walltime/WT]) :-!,
   statistics(runtime,[RE,_]),
   statistics(total_runtime,[TE,_]),
   statistics(walltime,[WE,_]),
   RT is RE-R, RTT is TE-T, WT is WE-W.




% ==========================================

%  TERM EXPANDER PART

:- meta_predicate assert_if_new(0).
assert_if_new(P) :- (P -> true ; assert(P)).

aflatten(List,FlatList) :- flatten1(List,[],FlatList).
flatten1([],L,L) :- !.
flatten1([H|T],Tail,List) :- !, flatten1(H,FlatList,List), flatten1(T,Tail,FlatList).
flatten1(NonList,Tail,[NonList|Tail]).

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

layout_sub_term([],N,[]) :- !, format('~n*** Could not obtain layout information (~w)~n',[N]).
layout_sub_term([H|T],N,Res) :- !,
    (N=<1 -> Res=H ; N1 is N-1, layout_sub_term(T,N1,Res)).
layout_sub_term(Term,N,Res) :-
    format('~n*** Virtual position: ~w~n',[layout_sub_term(Term,N,Res)]), % can happen when add_args adds new positions which did not exist
    Res=Term.


% for a term-expander context compute a position term; if not possible: constructs unknown
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
    assert_if_new(meta_call(CM,CP,VariableCall,ExtraArgs2,ClauseHead, StartLine, EndLine)).
assert_unresolved_meta_call(VariableCall,ExtraArgs,Layout,CallingPredicate,DCG,Info) :-
    format('*** ERROR: ~w~n',[assert_unresolved_meta_call(VariableCall,ExtraArgs,Layout,CallingPredicate,DCG,Info)]).

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
    assert_if_new(calling(CM,CP, Module,Name/Arity, StartLine, EndLine)).

adapt_arity(no_dcg,Arity,R) :- !, R=Arity.
adapt_arity(dcg,SourceArity,Arity) :- !,Arity is SourceArity+2.
%adapt_arity(meta(N),SourceArity,Arity) :- !,Arity is SourceArity+N.
adapt_arity(DCG,Arity,R) :- add_infolog_error(informat('unknown DCG type: ~w',[DCG])), R=Arity.

safe_analyze_body(X,Layout, CallingPredicate, DCG, Info) :-
   (analyze_body(X,Layout, CallingPredicate, DCG, Info) -> true
     ; get_position_term(CallingPredicate,Layout,PositionTerm),
       add_infolog_error(informat('analyze body failed: ~w',[analyze_body(X,Layout, CallingPredicate, DCG)]),PositionTerm)
       %,trace, analyze_body(X,Layout, CallingPredicate, DCG, Info)
    ).

% analyze_body(BODYTERM, LayoutInfo, CallingPredicate, DCGInfo)
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
    layout_sub_term(Layout,2,LayoutX),
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
    layout_sub_term(Layout,2,LayoutX),
    safe_analyze_body(X,LayoutX,CallingPredicate,DCG, Info).

analyze_body((A -> B ; C),Layout, CallingPredicate, DCG, Info) :-
    !,
    assert_call(CallingPredicate, built_in:'->'(_,_,_), Layout, DCG),
    layout_sub_term(Layout,2,LayoutAB),
    layout_sub_term(LayoutAB,2,LayoutA),
    layout_sub_term(LayoutAB,3,LayoutB),
    layout_sub_term(Layout,3,LayoutC),
    safe_analyze_body(A,LayoutA, CallingPredicate, DCG, Info),
    safe_analyze_body(B,LayoutB, CallingPredicate, DCG, Info),
    safe_analyze_body(C,LayoutC, CallingPredicate, DCG, Info).

analyze_body((A,B),Layout, CallingPredicate, DCG, Info) :-
  !,
  layout_sub_term(Layout,2,LayoutA),
  layout_sub_term(Layout,3,LayoutB),
  safe_analyze_body(A,LayoutA, CallingPredicate, DCG, Info),
  safe_analyze_body(B,LayoutB, CallingPredicate, DCG, Info).

analyze_body((A;B),Layout, CallingPredicate, DCG, Info) :-
  !,
  layout_sub_term(Layout,2,LayoutA),
  layout_sub_term(Layout,3,LayoutB),
  safe_analyze_body(A,LayoutA, CallingPredicate, DCG, Info),
  safe_analyze_body(B,LayoutB, CallingPredicate, DCG, Info).

analyze_body((A->B),Layout, CallingPredicate, DCG, Info) :-
  !,
  layout_sub_term(Layout,2,LayoutA),
  layout_sub_term(Layout,3,LayoutB),
  safe_analyze_body(A,LayoutA, CallingPredicate, DCG, Info),
  safe_analyze_body(B,LayoutB, CallingPredicate, DCG, Info).

analyze_body(OrigMETA, Layout, CallingPredicate, DCG, Info) :-
   (DCG=no_dcg -> META = OrigMETA
     ; add_args(OrigMETA,2,META)
   ),
   meta_pred(META,MODULE,List),
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
  (functor(Call,N,A) ->
    assert_call(CallingPredicate, Module:recursive_call, Layout, DCG) ;
    assert_call(CallingPredicate, module_yet_unknown:Call, Layout, DCG)).


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
     ; format('~nAdding meta_user_pred(~w,~w,~w)~n',[Head,Module,MetaArgList]),
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
meta_built_in_pred(call(_),built_in,[meta_arg(1,0),meta_arg(2,0)]).
meta_built_in_pred(call(_,_),built_in,[meta_arg(1,1)]).
meta_built_in_pred(call(_,_,_),built_in,[meta_arg(1,2)]).
meta_built_in_pred(call(_,_,_,_),built_in,[meta_arg(1,3)]).
meta_built_in_pred(call(_,_,_,_,_),built_in,[meta_arg(1,4)]).
meta_built_in_pred(once(_),built_in,[meta_arg(1,0)]).
% TO DO: support setof/3, bagof/3 arguments ^
% We could add ;/2, \+/1, ...


analyze_sub_arg(DModule:META,MODULE, Layout, CallingPredicate, Info, meta_arg(Nr,ADD) ) :- !,
  (DModule=MODULE -> true /* user provided additional module prefix; peel it off */
    ; format('*** Module prefix mismatch: ~w:~w (expected ~w)~n',[DModule,META,MODULE])),
   % we need to peel off layout to get to META:
   layout_sub_term(Layout,3,LayoutM),
   analyze_sub_arg(META, MODULE, LayoutM, CallingPredicate, Info, meta_arg(Nr,ADD) ).
analyze_sub_arg(META, _, Layout, CallingPredicate, Info, meta_arg(Nr,ADD) ) :- Nr1 is Nr+1,
  layout_sub_term(Layout,Nr1,LayoutA),
  arg(Nr,META,SubArg), %print(add_args(SubArg,Nr,SubArgADD)),nl,trace,
  add_args(SubArg,ADD,SubArgADD),
  %format(' Analyze Sub ~w -> ~w  [ ~w ] (from ~w)~n',[Nr,ADD,SubArgADD,CallingPredicate]),
  safe_analyze_body(SubArgADD,LayoutA, CallingPredicate, no_dcg, Info).

add_args(Call,0,Res) :- !, Res=Call.
add_args(Var,_N,Res) :- var(Var),!, Res=_.% causes problems with layout info: is_meta_call_n(Res,N,Var).
add_args(M:Call,N,Res) :- !, Res = M:CR, add_args(Call,N,CR).
add_args(Call,N,Res) :- %print(add(Call,N)),nl,
  Call =.. FA,
  length(Xtra,N), append(FA,Xtra,NFA),
   Res =.. NFA.




problem(X) :- problem(X,_).
mk_problem(P,Loc) :- assert(problem(P,Loc)).
mk_problem(P) :- assert(problem(P,unknown)).

%% analyzing Prolog Code

% exporting as binary fact
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


analyze((:- module(Name, ListOfExported)), _Layout, Module, File) :-
    !,
    x_unwrap_module(File,UnwrappedName),
    (Name = UnwrappedName -> true; mk_problem(wrong_filename(Module,Name,File))),
    (defined_module(Name2,File) -> mk_problem(multiple_modules_in_file(File, Name, Name2)); true),
    retractall(defined_module(Name,_)),
    assert_if_new(defined_module(Name,File)),
    maplist(add_fact2(is_exported, Name),ListOfExported).

analyze((:- use_module(UsedModule, ListOfImported)), _Layout,Module, _File) :- % IMPORTS
    !, dependency(Module,UsedModule),
    x_unwrap_module(UsedModule,UnwrappedUsedName),
    x_unwrap_module(Module,UnwrappedName),
    maplist(add_fact3(is_imported,UnwrappedName,UnwrappedUsedName),ListOfImported). % TO DO: add source location

analyze((:- use_module(X)), _Layout, Module, _File) :-
    (is_list(X) -> maplist(dependency(Module),X); dependency(Module,X)).
analyze((:- prob_use_module(X)), _Layout, Module, _File) :-
    (is_list(X) -> maplist(dependency(Module),X); dependency(Module,X)).

analyze((:- dynamic(X)), _Layout,Module,_File) :-
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_dynamic, Module),L).

analyze((:- public(X)), _Layout,Module,_File) :-
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_public, Module),L).

analyze((:- meta_predicate(X)), _Layout,Module, _File) :-
    !,
    pairs_to_list(X,L),
    maplist(add_fact(is_meta, Module), L),
    maplist(add_meta_predicate(Module), L).

%blocking, operator declarations, volatile, multifile, 	mode

analyze((:- mode(X)), _Layout, Module, _File) :-
    !,
    pairs_to_list(X,L),
    maplist(add_fact(declared_mode, Module), L).

analyze((:- block(X)), _Layout, Module, _File) :-
    !,
    pairs_to_list(X,L),
    maplist(add_fact(is_blocking, Module), L).

analyze((:- op(Priority,FixityTerm,Name)), _Layout,Module, _File) :-
  fixity(FixityTerm, Fixity, Associativity, Arity),
  assert_if_new(predicate(Module,Name/Arity)),
  assert_if_new( operator(Module:Name/Arity,Priority,Fixity,Associativity) ).

analyze((:- volatile(X)), _Layout,Module, _File) :-
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_volatile, Module),L).

analyze((:- chr_constraint(X)), _Layout,Module, _File) :-
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_chr_constraint, Module),L).

analyze((:- attribute(X)), _Layout,Module, _File) :- depends_on(Module,atts),
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_attribute, Module),L).

analyze((:- chr_option(_,_)), _Layout, _Module, _File). % just ignore for analysis

analyze((:- multifile(X)), _Layout, Module, _File) :-
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_multifile, Module),L).

analyze(':-'(Body), Layout, Module, _File) :- %portray_clause(query(Body,Layout)),
    !,
    layout_sub_term(Layout,2,LayoutSub),
    safe_analyze_body(Body,LayoutSub,Module:':-'/1, no_dcg, [query]). % TO DO: check why this fails

analyze((Head :- Body), Layout, Module, _File) :-
    !, %portray_clause((Head :- Body)),
    functor(Head,Name,Arity),
    % nl,nl,print(layout_clause(Name/Arity,Head,Body)),nl,
    Predicate = Module:Name/Arity,
    layout_sub_term(Layout,2,LayoutHead),
    assert_head(Predicate, LayoutHead),
    layout_sub_term(Layout,3,LayoutSub),
    % (Name=force_non_empty -> trace ; true),
    safe_analyze_body(Body,LayoutSub, Predicate, no_dcg,[head/Head]).

analyze((Head --> Body), Layout, Module, _File) :- %portray_clause((Head --> Body)),
    !,
    functor(Head,Name,WrongArity),
    Arity is WrongArity + 2,
    Predicate = Module:Name/Arity,
    layout_sub_term(Layout,2,LayoutHead),
    assert_head(Predicate, LayoutHead),
    layout_sub_term(Layout,3,LayoutSub),
    safe_analyze_body(Body,LayoutSub, Predicate, dcg, [head/Head]). % TO DO: add two args to Head ?


analyze(runtime_entry(_), _L, _M, _F) :- !.
analyze(end_of_file, _L, _M, _F) :- !.

analyze(Fact, Layout, Module, _File) :- %portray_clause( Fact ),
    !,
    %nl,print(fact(Fact)),nl,
    functor(Fact,Name,Arity),
    Predicate = Module:Name/Arity,
    assert_head(Predicate, Layout).


% analyzef/5
analyzef(foreign(Name, PredSpec), Layout, Module, _File, (:- dynamic(Name/Arity))) :-
    !,
    functor(PredSpec,_,Arity),
    Predicate = Module:Name/Arity,
    assert_head(Predicate, Layout).

analyzef(foreign(Name, _Lang, PredSpec), Layout, Module, _File, TermOut) :-
    analyzef(foreign(Name, PredSpec), Layout, Module, _File, TermOut).


assert_head(Predicate, Layout) :-
    decompose_call(Predicate,M,P),
    assert_if_new(predicate(M,P)),
    get_position(Layout, StartLine, EndLine),
    assert(klaus(M,P,  StartLine, EndLine)).

decompose_call(M:P,MR,PR) :- !,decompose_call2(P,M,MR,PR). % peel off all : module constructors
decompose_call(P,module_yet_unknown,P) :- format('*** Unknown Module for ~w~n',[P]).

decompose_call2(P,OuterModule,MR,PR) :- var(P),!, (MR,PR)=(OuterModule:P).
decompose_call2(M:P,_OuterModule,MR,PR) :- !, decompose_call2(P,M,MR,PR).
decompose_call2(P,OuterModule,OuterModule,P).

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

get_module(Name, Arity, CallingModule, undefined_module, Loc) :-
  mk_problem(could_not_infer_module(Name, Arity, CallingModule),Loc).


update :-
    retract(calling(CallingModule,CallingPred, module_yet_unknown,Name/Arity, Start, End)),
    get_module(Name, Arity, CallingModule, Module, module_pred_lines(CallingModule,CallingPred,Start,End)),
    assert_if_new(predicate(Module,Name/Arity)),
    assert_if_new(calling(CallingModule,CallingPred, Module,Name/Arity, Start, End)),
    fail.
update.

:- multifile user:term_expansion/6.
:- dynamic seen_token/0.

:- prolog_flag(compiling,_,debugcode).
:- prolog_flag(source_info,_,on).
%:- prolog_flag(profiling,_,on).
:- prolog_flag(redefine_warnings,_,off).

user:term_expansion(Term, Layout, Tokens, TermOut, Layout, [codeq | Tokens]) :-
    %print(d(Term, Tokens)),nl,
    %(Term = (atomic_eq_check(_,_,_) :- B) -> trace ; true),
    nonmember(codeq, Tokens), % do not expand if already expanded
    prolog_load_context(module, Module),
    prolog_load_context(file, File),
    (member(rm_debug_calls,Tokens) -> assert_if_new(seen_token); true),
    %(seen_token -> member(rm_debug_calls,Tokens) ; true), % I am not sure what the purpose of this is ? It certainly removes certain clauses from the analysis
  % print(expand(Module,Term)),nl,
    (  analyzef(Term, Layout, Module, File, TermOut)
     ; analyze(Term, Layout, Module, File), (Term=portray_message(informational,_) -> TermOut = '$ignored'(Term) ; TermOut = Term)),
    !.

%% KNOWN ISSUES

%%% Infolog cannot deal with calls of the form MODULE:(A,B)
%%% attribute usage is not checked
%%% tcltk_call does not store arity
%%% lint does not cache; lint_for_module computes almost all errors/warning

%% TO DO:

%% DETECT unnecessary unification foldl(Module:Pred,List,Start,Result) :- foldl2(List,Module:Pred,Start,Result).
