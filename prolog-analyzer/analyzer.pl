:- prolog_flag(compiling,_,debugcode).
:- prolog_flag(source_info,_,on).
:- prolog_flag(profiling,_,on).
portray_message(informational, _).

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(system)).
:- use_module(library(file_systems)).
:- use_module(library(codesio)).
:- use_module(library(process)).

:- use_module(escaper).



% :- op(300, fy, ~~).

% THE DYNAMIC PREDICATES WHICH ARE PART OF THE ANALYSIS

:-  dynamic
    defined_module/2,% module(name,file)
    predicate/2,     % predicate(module,name/arity)
    is_dynamic/2,    % is_dynamic(module,name/arity)
    is_volatile/2,   % is_volatile(module,name/arity)
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
    problem/1.       % problem(details)


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

calling(M1:C1,M2:C2) :- calling(M1,C1,M2,C2,_,_).
calls_path(Call1,Call2,Path) :- transitive(calling(Call1,Call2),Path).

% instantiate module dependency path with call witnesses
instantiate([_]).
instantiate([A,B|T]) :- calling(A:C1,B:C2),!,
    format('Module ~w -> ~w   [call: ~w -> ~w ]~n',[A,B,C1,C2]),
    instantiate([B|T]).
instantiate([A,B|_T]) :- format('*** Vacuous Module Dependency: ~w -> ~w~n',[A,B]),fail. % probably because of a a call in a :- declaration ?!?
% TO DO: probably also analyse :- directives


lint :- print('Start checking'),nl,lint(_).
lint(vacuous_modules) :-
        vacuous_module_dependency(_M1,_M2),fail.
lint(uncovered_calls) :-
        uncovered_call(FromModule,FromQ,ToModule,Call,L1,L2),
        format('*** Uncovered Call in module ~w: ~w:~w [~w - ~w, (~w)]~n',[FromModule,ToModule,Call,L1,L2,FromQ]),
        fail.
lint(missing_meta_predicates) :-
        uncovered_meta_call(FromModule,Pred,L1,L2,Msg),
        format('*** Missing meta_predicate annotation (~w) for ~w:~w [lines ~w-~w]~n',[Msg,FromModule,Pred,L1,L2]),
        fail.
lint(_) :- print('Done checking'),nl.

% check if there is a dependency without a call that requires it:
vacuous_module_dependency(M1,M2) :- depends_on(M1,M2),
   \+ calling(M1:_,M2:_),
   format('*** Vacuous Module Dependency: ~w -> ~w~n',[M1,M2]).

% is there a cycle in the module dependency graph:
cycle(Module,ModulePath) :-
   depends_path(Module,Module,ModulePath),
   instantiate(ModulePath).

% is there a calling cycle which leaves a module and enters it again ?
cross_module_cycle(Module,Call,[Module:Call|Path]) :-
    calling(Module,Call,TargetModule,TargetCall,_L1,_L2),
    TargetModule \= Module,
    calls_path(TargetModule:TargetCall,Module:Call,Path),
    instantiate(Path).

print_calls(FromModule,ToModule) :-
   defined_module(FromModule,_),
   format('Calls from ~w to ~w~n===================~n',[FromModule,ToModule]),
   calling(FromModule,C1,ToModule,C2,L1,L2),
   format('Call ~w  ->  ~w   [lines: ~w - ~w]~n',[C1,C2,L1,L2]),
   fail.
print_calls(_,_) :- format('===================~n',[]).

% try and find calls where the predicate is not annotated with a meta_predicate
uncovered_meta_call(FromModule,Pred,L1,L2,Msg) :-
   meta_call(FromModule,Pred,XX,NrAddedArgs,Head,L1,L2),
   (meta_pred_functor(Pred,FromModule,MetaList)
     -> Head =.. [_|Args], XX='$CALL',
        nth1(Nr,Args,Arg), Arg==XX,
        nonmember(meta_arg(Nr,NrAddedArgs),MetaList),
        Msg = arg(Nr,NrAddedArgs)
        , print(missing_arg(Nr,NrAddedArgs,MetaList,Pred,Head)),nl
     ;  Msg = no_annotation).
   
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
    \+ klaus(ToModule,Call,_,_),
    \+ always_defined(Call),
    \+ is_dynamic(ToModule,Call),
    \+ check_imported(ToModule,Call,FromModule).

check_imported(built_in,_Call,_) :- !. % assume built-in exists; TO DO: check ?
check_imported(Module,Call,FromModule) :- is_imported(Module,Call,FromModule), % selectively imported
   !,
   (standard_module(Module) -> true % ASSUME OK; TO DO: Check more rigourously
     ; is_exported(Module,Call) -> true
     ; format('Importing call which is not exported: ~w:~w~n',[Module,Call]),fail
    ).
check_imported(Module,Call,FromModule) :- depends_on(FromModule,Module), % imported, but not selectively imported
   (standard_module(Module) -> true % ASSUME OK; TO DO: Check more rigourously
     ; is_exported(Module,Call)
    ).
   

% always defined
always_defined(recursive_call/0).
always_defined(put_atts/2).
always_defined(get_atts/2).
always_defined('~~'/1).  % ProB specific term expander; TO DO: get rid of this
always_defined(F/N) :- functor(Call,F,N), meta_pred(Call,built_in,_).

% to do: more precise analysis of which predicates are actually exported
standard_module(aggregate).
standard_module(assoc).
standard_module(avl).
standard_module(between).
standard_module(built_in).
standard_module(clpfd).
standard_module(codesio).
standard_module(fastrw).
standard_module(file_systems).
standard_module(heaps).
standard_module(lists).
standard_module(ordsets).
standard_module(process).
standard_module(random).
standard_module(samsort).
standard_module(sets).
standard_module(system).
standard_module(tcltk).
standard_module(terms).
standard_module(timeout).
standard_module(xml).


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

repl :-
  read(Term),
  Term =.. [_|Args],
  exclude(ground,Args,VarArgs),
  (call(Term) ->
    (nl,write(result(VarArgs)),nl,write(cljdone),nl);
    (nl,write(result(no)),nl,write(cljdone),nl)),
  repl.
%% Entry-point: analyze("/path/to/prob/src/prob_tcltk.pl", "name of clojure output")

analyze(InputFile,OutputFile) :-
    analyze(InputFile),
    print(exporting(OutputFile)),nl,
    start_analysis_timer(T3),
    export_to_clj_file(OutputFile),
    stop_analysis_timer(T3).

analyze(InputFile) :-
    print('loading modules'),nl,
    start_analysis_timer(T1),
    use_module(InputFile),
    nl,
    stop_analysis_timer(T1),
    nl, print('updating calls'), nl,
    start_analysis_timer(T2),
    update,
    stop_analysis_timer(T2),
    nl.

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

% export analysis results to clojure file:

export_to_clj_file(OutputFile) :-
    open(OutputFile,write,Stream),
    export(Stream),
    flush_output(Stream),
    close(Stream).


export(Stream) :-
 export_all(Stream).

% read all characters from a stream
stream2code(S,Atom) :-
  read_line(S,Text),
  atom_codes(Atom,Text).

git_revision(Sha) :-
   absolute_file_name('$SHELL', Shell),
   process_create(Shell, ['-c','cd $PROB_HOME && git rev-parse HEAD && cd - >/dev/null'],[stdout(pipe(F)), process(P)]),
   process_wait(P,_ExitCode),
   stream2code(F,Sha).


export_all(S) :-
 git_revision(Sha),

 format(S, '[ ~n',[]),
 format(S, '[git "~a"]~n',[Sha]),

 export_X2(S,predicate),
 export_X2(S,is_dynamic),
 export_X2(S,is_volatile),
 export_X2(S,is_exported),
 export_X2(S,is_multifile),
 export_X2(S,is_meta),
 export_X2(S,declared_mode),
 export_X2(S,is_blocking),

 export_defined_modules(S),
 export_clause(S),
 export_calling(S),
 export_operator(S),
 export_problems(S),
 export_dependencies(S),
 format(S, ']~n',[]).


export_dependencies(S) :-
   (depends_on(LM,IM),
    write_clojure(S,dependency,[LM,string,IM,string]),fail
    ; true).

export_defined_modules(S) :-
  (defined_module(M,File), write_clojure(S,module,[M,string, File,string]),fail
   ; true).

export_problems(S) :-
  findall([P], problem(P),L),
  maplist(escaping_format(S,'[problem "~w"]~n'),L).

export_clause(S) :-
   (klaus(M,P/A, Start, End),
    write_clojure(S,clause,[M,string, P,string, A, number, Start, number, End, number]), fail
    ; true).

export_operator(S) :-
  (operator(M:P/A,Prio, Fix, Assoc),
   write_clojure(S, operator,[M,string,  P,string,  A, number,
            Prio, number,  Fix, string,  Assoc, string]),fail
    ; true).

export_calling(S) :-
  (calling(M,P/A, CM,CP/CA, Start, End), 
   write_clojure(S,call,[M,string, P,string, A, number,
            CM, string, CP, string, CA, number, Start, number, End, number]), fail
    ; true).


%clojure_fact_wrap(S,X,E) :-
% format(S,'[ ~a ',[X]),
% call(E),
% format(S,']~n',[]).


export_X1(S, X) :-
  (call(X,M:P/A),write_clojure(S,X,[M,string, P,string, A, number]),fail ; true),!.

export_X2(S, X) :-
   (call(X,M:P/A,Args),
    write_clojure(S,X,[M,string, P,string, A, number,  Args, string]),fail ; true).

write_clojure(S,X,E) :-
 format(S,'[ ~a ',[X]),
 %clojure_fact_wrap(S,X,write_clojure2(S,X,E)),
 write_clojure2(E,S,X),
 format(S,']~n',[]).

write_clojure2([],_,_).
write_clojure2([Content,Type|T],S,X) :-
  write_clojure_type(Type, S,X, Content),
  write_clojure2(T,S,X).

write_clojure_type(number, S,_, Content) :- !,
  format(S, '~d ',[Content]).
%default type is string
write_clojure_type(_, S,_, Content) :-
  escaping_format(S, '"~w" ',[Content]).

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
layout_sub_term(Term,N,Res) :- format('~n*** Illegal layout: ~w~n',[layout_sub_term(Term,N,Res)]), Res=[].

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
      ; format('*** assert_call failed ~w~n',[assert_call(CallingPredicate, Predicate, Layout, DCG)])).
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
adapt_arity(DCG,Arity,R) :- format('*** Unknown DCG type: ~w~n',[DCG]), R=Arity.

safe_analyze_body(X,Layout, CallingPredicate, DCG, Info) :-
   (analyze_body(X,Layout, CallingPredicate, DCG, Info) -> true
     ; format('~n**** Analyze body failed: ~w~n~n',[analyze_body(X,Layout, CallingPredicate, DCG)])
     ,trace, analyze_body(X,Layout, CallingPredicate, DCG, Info)
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



:- use_module(meta_pred_generator,[translate_meta_predicate_pattern/3]).
:- dynamic meta_user_pred/3.
:- include(meta_user_pred_cache). % cached version from previous run
add_meta_predicate(Module,Pattern) :-
   translate_meta_predicate_pattern(Pattern,Head,MetaArgList),
   (meta_user_pred(Head,Module,MetaArgList) -> true
     ; format('~nAdding meta_user_pred(~w,~w,~w)~n',[Head,Module,MetaArgList]),
       assert(meta_user_pred(Head,Module,MetaArgList))
    ).

% write meta_user_pred facts
gen_user :- % tell meta_user_pred_cache.pl
     meta_user_pred(H,M,L), portray_clause(meta_user_pred(H,M,L)),nl,fail.
gen_user.

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
meta_built_in_pred(if(_,_,_),built_in,[meta_arg(1,0),meta_arg(2,0),meta_arg(3,0)]).
%meta_built_in_pred(( _ -> _), built_in,[meta_arg(1,0),meta_arg(2,0)]). % dealt with specially in DCG mode
meta_built_in_pred(findall(_,_,_),built_in,[meta_arg(2,0)]).
meta_built_in_pred(findall(_,_,_,_),built_in,[meta_arg(2,0)]).
meta_built_in_pred(assert(_),built_in,[meta_arg(1,0)]). % TO DO: keep more info about which predicate asserted
meta_built_in_pred(asserta(_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(assertz(_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(retract(_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(retractall(_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(call(_),built_in,[meta_arg(1,0)]).
meta_built_in_pred(call(_,_),built_in,[meta_arg(1,1)]).
meta_built_in_pred(call(_,_,_),built_in,[meta_arg(1,2)]).
meta_built_in_pred(call(_,_,_,_),built_in,[meta_arg(1,3)]).
meta_built_in_pred(call(_,_,_,_,_),built_in,[meta_arg(1,4)]).
% TO DO: support setof/3, bagof/3
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





mk_problem(P) :- assert(problem(P)).

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

% utilities to construct/deconstruct terms; faster than =..
unop(X,P,A1) :- functor(X,P,1), arg(1,X,A1).
binop(X,P,A1,A2) :- functor(X,P,2), arg(1,X,A1), arg(2,X,A2).
ternop(X,P,A1,A2,A3) :- functor(X,P,3), arg(1,X,A1), arg(2,X,A2), arg(3,X,A3).

pairs_to_list((X,Y), [X|R]) :- pairs_to_list(Y,R).
pairs_to_list(X, [X]).


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


x_unwrap_module(chrsrc(X),Y) :- !, X=Y.
x_unwrap_module(library(X),Y) :- !, X=Y.
x_unwrap_module(probsrc(X),Y) :- !, X=Y.
x_unwrap_module(probcspsrc(X),Y) :- !, X=Y.
x_unwrap_module(bparser(X),Y) :- !, X=Y.
x_unwrap_module(plugins(X),Y) :- !, X=Y.
x_unwrap_module(abstract_domains(X),Y) :- !, X=Y.
x_unwrap_module(tclsrc(X),Y) :- !, X=Y.
x_unwrap_module(smt_solvers_interface(X),Y) :- !, X=Y.
x_unwrap_module(probporsrc(X),Y) :- !, X=Y.
x_unwrap_module(prozsrc(X),Y) :- !, X=Y.
x_unwrap_module(probltlsrc(X),Y) :- !, X=Y.
x_unwrap_module(probpgesrc(X),Y) :- !, X=Y.
x_unwrap_module(extension(E),Y) :- !,
    atom_chars(E,ExtensionPath),
    suffix(ExtensionPath,Module),
    atom_chars(Y,Module).
x_unwrap_module(Path,X) :-
    atom_chars(Path,PathChars),
    ( append(Base,[.,p,l],PathChars),
      suffix(Base,XChars)  % module loaded with .pl ending
    ; suffix(PathChars,XChars)), % or without
    x_remove_path(XChars,CharsWithoutPath),
    atom_chars(X,CharsWithoutPath).
x_unwrap_module(X,X) :- !. % might even be unwrapped

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
    maplist(add_fact3(is_imported,UnwrappedName,UnwrappedUsedName),ListOfImported).

analyze((:- use_module(X)), _Layout, Module, _File) :-
    (is_list(X) -> maplist(dependency(Module),X); dependency(Module,X)).

analyze((:- dynamic(X)), _Layout,Module,_File) :-
       !,
       pairs_to_list(X,L),
       maplist(add_fact2(is_dynamic, Module),L).

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

get_module(Name, Arity, CallingModule, built_in) :-
   functor(Call, Name, Arity),
   predicate_property(CallingModule:Call,built_in),!.

get_module(Name, Arity, CallingModule, Module) :-
   functor(Call, Name, Arity),
   predicate_property(CallingModule:Call,imported_from(Module)),!.


get_module(Name, Arity, CallingModule, CallingModule) :-
   functor(Call, Name, Arity),
   (predicate_property(CallingModule:Call,interpreted); predicate_property(CallingModule:Call,compiled)),!.

get_module(Name, Arity, CallingModule, undefined_module) :-
  mk_problem(could_not_infer_module(Name, Arity, CallingModule)).


update :-
    retract(calling(CallingModule,CallingPred, module_yet_unknown,Name/Arity, Start, End)),
    get_module(Name, Arity, CallingModule, Module),
    assert_if_new(predicate(Module,Name/Arity)),
    assert_if_new(calling(CallingModule,CallingPred, Module,Name/Arity, Start, End)),
    fail.
update.

:- multifile user:term_expansion/6.


:- dynamic seen_token/0.

user:term_expansion(Term, Layout, Tokens, TermOut, Layout, [codeq | Tokens]) :-
    %print(d(Term, Tokens)),nl,
    %(Term = (atomic_eq_check(_,_,_) :- B) -> trace ; true),
    nonmember(codeq, Tokens), % do not expand if already expanded
    prolog_load_context(module, Module),
    prolog_load_context(file, File),
    (member(rm_debug_calls,Tokens) -> assert_if_new(seen_token); true),
    %(seen_token -> member(rm_debug_calls,Tokens) ; true), % I am not sure what the purpose of this is ? It certainly removes certain clauses from the analysis
  % print(expand(Module,Term)),nl,
    (analyzef(Term, Layout, Module, File, TermOut) ; (analyze(Term, Layout, Module, File), TermOut = Term)),
    !.
