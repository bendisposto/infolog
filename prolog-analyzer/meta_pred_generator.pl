:- module(meta_pred_generator,[gen/0, translate_meta_predicate_pattern/3]).


% generate facts of the form:
% meta_pred(maplist(_,_),lists,[meta_arg(1,1)]).
% for annotations copied from SICStus libraries
% (Motivation: these facts enable indexing and faster analysis in analyzer.pl

gen :- % tell(meta_preds.pl),
   format(':- module(meta_preds,[meta_library_pred/3]).~n',[]),
   meta_predicate(Module,List),
   format('~n% module ~w~n~n',[Module]),
   member(Pattern,List),
   translate_meta_predicate_pattern(Pattern, Head,MetaArgList),
   portray_clause( meta_library_pred(Head,Module,MetaArgList)),
   fail.
gen :-nl.

translate_meta_predicate_pattern(Pattern, Head, MetaArgList) :-
   functor(Pattern,Predicate,N),
   Pattern =.. [_|Args],
   gen_meta_arg_list(Args,1,MetaArgList),
   functor(Head,Predicate,N). % create copy of Pattern with vars as args
    
gen_meta_arg_list([],_,[]).
gen_meta_arg_list([N|T],Pos,[meta_arg(Pos,N)|MT]) :- number(N),!,
    P1 is Pos+1,
    gen_meta_arg_list(T,P1,MT).
gen_meta_arg_list([_|T],Pos,MT) :- P1 is Pos+1, gen_meta_arg_list(T,P1,MT).


% -----------------------

% meta_predicate annotations of SICStus libraries:

meta_predicate( lists,
  [
	cumlist(3, +, ?, ?),
	cumlist(4, ?, ?, ?, ?),
	cumlist(5, ?, ?, ?, ?, ?),
	maplist(1, ?),
	maplist(2, ?, ?),
	maplist(3, ?, ?, ?),
	map_product(3, +, +, ?),
	scanlist(3, +, ?, ?),
	scanlist(4, ?, ?, ?, ?),
	scanlist(5, ?, ?, ?, ?, ?),
	some(1, ?),
	some(2, ?, ?),
	some(3, ?, ?, ?),
	somechk(1, +),
	somechk(2, +, +),
	somechk(3, +, +, +),
	convlist(2, +, ?),
	exclude(1, +, ?),
	exclude(2, +, +, ?),
	exclude(3, +, +, +, ?),
	include(1, +, ?),
	include(2, +, +, ?),
	include(3, +, +, +, ?),
	partition(2, +, ?, ?, ?),
	    partition_1(+, 2, ?, ?, ?),
	        partition_1(+, +, ?, ?, ?, +, 2),
	group(2, +, ?),
	    group1(+, ?, 2),
	group(1, +, ?, ?),
	group(2, +, +, ?, ?),
	ordered(2, +),
	    ordered_0(+, 2),
	    ordered_1(+, +, 2),
	max_member(2, +, ?),
	min_member(2, +, ?),
	select_min(2, ?, +, ?),
	    sel_min_gen(+, ?, 2, ?),
		sel_min_gen(+, ?, +, 2, -, ?),
	select_max(2, ?, +, ?),
	    sel_max_gen(+, ?, 2, ?),
		sel_max_gen(+, ?, +, 2, -, ?),
	decreasing_prefix(2, +, ?, ?),
	first_precedes(2, +, +),
	increasing_prefix(2, +, ?, ?),
	precedes_first(2, +, +)
	]).

meta_predicate( avl,
 [
        %% [PM] 4.2.1 FIXME: The '?' positions are '-' in the documentation.
	avl_map(1, ?),
	avl_map(2, ?, ?) ]).


meta_predicate( samsort,
	[merge(2, +, +, ?),
	    sam_merge(+, +, 2, ?),
	samsort(2, +, ?),
	    sam_sort(+, 2, +, +, ?),
		sam_run(+, +, +, 2, -, -),
		    sam_rest(+, +, +, +, 2, -, -),
		sam_fuse(+, 2, ?),
		sam_fuse(+, +, 2, +, ?) ]).


meta_predicate(assoc,
	[map_assoc(1, +),
	map_assoc(2, +, -)]).


meta_predicate(queues,
	[map_queue(1, ?),
	map_queue(2, ?, ?),
	map_queue_list(2, ?, ?),
	map_list_queue(2, ?, ?),
	some_queue(1, ?),
	some_queue(2, ?, ?),
	somechk_queue(1, ?),
	somechk_queue(2, ?, ?)]).