:- module(library_modules,[precompile_library_modules/0,
     is_exported_by_library/2, library_export_list_available/1,
     is_library_module/1,
     library_module/2,
     private_library_predicate/2 ]).

:- dynamic is_exported_by_library/2.
:- dynamic library_export_list_available/1.

is_exported_by_library(_,_) :- print('**** LIBRARY EXPORT NOT PRECOMPILED ***'),nl,nl,fail.

precompile_library_modules :- retractall(is_exported_by_library(_,_)),
   retractall(library_export_list_available(_)),
   library_module(M,List),
   assert(library_export_list_available(M)),
   member(P,List),
   assert(is_exported_by_library(M,P)),fail.
precompile_library_modules.

is_library_module(aggregate).
is_library_module(assoc).
is_library_module(avl).
is_library_module(bags).
is_library_module(between).
is_library_module(built_in).
is_library_module(clpfd).
is_library_module(codesio).
is_library_module(fastrw).
is_library_module(file_systems).
is_library_module(heaps).
is_library_module(lists).
is_library_module(ordsets).
is_library_module(plunit).
is_library_module(process).
is_library_module(random).
is_library_module(rem).
is_library_module(samsort).
is_library_module(sets).
is_library_module(sockets).
is_library_module(system).
is_library_module(system3).
is_library_module(tcltk).
is_library_module(terms).
is_library_module(timeout).
is_library_module(trees).
is_library_module(ugraphs).
is_library_module(wgraphs).
is_library_module(varnumbers).
is_library_module(xml).

% a few private predicates, which we use:
private_library_predicate(avl,avl_shrinkage/3).
private_library_predicate(avl,avl/6).
private_library_predicate(avl,avl_del_max/5).
private_library_predicate('$fd_delete'/3).

%% THE module headers from SICStus / Quintus Prolog libraries

library_module(aggregate, [
	forall/2,
	foreach/2,
	aggregate/3,
	aggregate/4,
	aggregate_all/3,
	aggregate_all/4,
	free_variables/4,
	term_variables/3
   ]).
library_module(assoc, [
	empty_assoc/1,		% Assoc ->
	assoc_to_list/2,	% Assoc -> List
	gen_assoc/3,		% Key x Assoc x Val
	get_assoc/3,		% Key x Assoc -> Val
	get_next_assoc/4,	% Key x Assoc -> Key x Val
	get_prev_assoc/4,	% Key x Assoc -> Key x Val
	is_assoc/1,		% Assoc ->
	list_to_assoc/2,	% List -> Assoc
	ord_list_to_assoc/2,	% List -> Assoc
	map_assoc/2,		% Pred x Assoc ->
	map_assoc/3,		% Pred x Assoc -> Assoc
	max_assoc/3,		% Assoc -> Key x Val
	min_assoc/3,		% Assoc -> Key x Val
	portray_assoc/1,	% Assoc ->
	put_assoc/4		% Key x Assoc x Val -> Assoc
   ]).
%% [PM] 4.1.3 attribute/1 is not exported, instead it is special-cased by a term expansion. For SPIDER we instead fake an exported definition.
library_module(attributes, [atts_subset/3, put_atts/2, get_atts/2,
                       %% Add missing export for SPIDER
                       (attribute)/1]).
library_module(avl, [
	avl_to_list/2,			% AVL -> List
	is_avl/1,			% AVL ->
	avl_change/5,			% Key -> (AVL x Val <-> AVL x Val)
	avl_domain/2,			% AVL -> OrdSet
	avl_fetch/2,			% Key x AVL ->
	avl_fetch/3,			% Key x AVL -> Val
	avl_height/2,			% AVL -> Height
	avl_incr/4,			% Key x AVL x Inc -> AVL
	avl_max/2,			% AVL -> Key
	avl_max/3,			% AVL -> Key x Val
	avl_member/2,			% Key x AVL
	avl_member/3,			% Key x AVL x Val
	avl_min/2,			% AVL -> Key
	avl_min/3,			% AVL -> Key x Val
	avl_next/3,			% Key x AVL -> Key
	avl_next/4,			% Key x AVL -> Key x Val
	avl_prev/3,			% Key x AVL -> Key
	avl_prev/4,			% Key x AVL -> Key x Val
	avl_range/2,			% AVL -> OrdSet
	avl_size/2,			% AVL -> Size
	avl_store/4,			% Key x AVL x Val -> Val
	avl_delete/4,			% Key x AVL x Val -> AVL
	avl_del_max/4,			% AVL -> Key x Val x AVL
	avl_del_min/4,			% AVL -> Key x Val x AVL
	avl_map/2,			% Goal x AVL ->
	avl_map/3,			% Goal x AVL -> AVL
	empty_avl/1,			% -> AVL
	list_to_avl/2,			% List -> AVL
	ord_list_to_avl/2,		% List -> AVL
	portray_avl/1			% AVL ->
   ]).
library_module(bags, [
	bag_add_element/3,
	bag_add_element/4,
	bag_del_element/3,
	bag_del_element/4,
	bag_intersect/2,
	bag_intersection/2,
	bag_intersection/3,
	bag_max/2,
	bag_max/3,
	bag_min/2,
	bag_min/3,
	bag_subtract/3,
	bag_to_list/2,
	bag_to_ord_set/2,
	bag_to_ord_set/3,
	bag_to_set/2,
	bag_to_set/3,
	bag_union/2,
	bag_union/3,
	checkbag/2,
	empty_bag/1,
	is_bag/1,
	length/3,
	list_to_bag/2,
	make_sub_bag/2,
	mapbag/2,
	mapbag/3,
	member/3,
	memberchk/3,
	portray_bag/1,
	somebag/2,
	somechkbag/2,
	test_sub_bag/2
   ]).
library_module(between, [
	between/3,		%   Lower x Upper x Bounded
	gen_int/1,		%   Integer
	gen_nat/1,		%   Natural
	numlist/2,		%   Upper -> List
	numlist/3,		%   Lower x Upper -> List
	numlist/5,		%   Lower x Step x Upper x Length -> List
	repeat/1		%   Natural
   ]).
library_module(clpfd, [
	% enumeration
	indomain/1,
	labeling/2,
	solve/2,
	first_bound/2,
	later_bound/2,
	minimize/2,
	minimize/3,
	maximize/2,
	maximize/3,
	/* order_resource/2, */
	% reflection
	fd_var/1,
	fd_min/2,
	fd_max/2,
	fd_size/2,
	fd_set/2,
	fd_dom/2,
	fd_degree/2,
	fd_statistics/0,
	fd_statistics/2,
	fd_neighbors/2,
	fd_closure/2,
	% constraints
	domain/3,
	iff/2,					% for compatibility
	in/2,
	in_set/2,
	all_different/1,
	all_different/2,
	all_distinct/1,
	all_distinct/2,
	element/3,
	relation/3,		% deprecated
	minimum/2,
	maximum/2,
	nvalue/2,
        geost/2,
        geost/3,
        geost/4,
	circuit/1,
	circuit/2,
	assignment/2,
	assignment/3,
	cumulative/1,
	cumulative/2,
        disjoint1/1,
        disjoint1/2,
        disjoint2/1,
        disjoint2/2,
        case/3,
        case/4,
        table/2,
        table/3,
	cumulatives/2,
	cumulatives/3,
	global_cardinality/2,
	global_cardinality/3,
	count/4,		% deprecated
	sum/3,
	scalar_product/4,
	scalar_product/5,
	sorting/3,
	keysorting/2,
	keysorting/3,
	lex_chain/1,
	lex_chain/2,
        automaton/3,
        automaton/8,
        automaton/9,
        smt/1,
        bool_and/2,
        bool_or/2,
        bool_xor/2,
        bool_channel/4,
	multi_cumulative/2,
	multi_cumulative/3,
	#= /2,
	#\= /2,
	#< /2,
	#=< /2,
	#> /2,
	#>= /2,
	#\ /1,
	#/\ /2,
	#\ /2,
	#\/ /2,
	#=> /2,
	#<= /2,
	#<=> /2,
	% geost extras
	% geost_domination_data/3,
	% geost_domination_post/4,
	% programming interface
	fd_batch/1,
	fd_global/3,
	fd_global/4,
	fd_flag/3,
	is_fdset/1,
	empty_fdset/1,
	fdset_parts/4,
	empty_interval/2,
	fdset_interval/3,
	fdset_singleton/2,
	fdset_min/2,
	fdset_max/2,
	fdset_size/2,
	list_to_fdset/2,
	fdset_to_list/2,
	range_to_fdset/2,
	fdset_to_range/2,
	fdset_add_element/3,
	fdset_del_element/3,
	fdset_disjoint/2,
	fdset_intersect/2,
	fdset_intersection/3,
	fdset_intersection/2,
	fdset_member/2,
	fdset_eq/2,
	fdset_subset/2,
	fdset_subtract/3,
	fdset_union/3,
	fdset_union/2,
	fdset_complement/2
		 ]).
library_module(codesio, [
	format_to_codes/3,
	format_to_codes/4,
	write_to_codes/2,
	write_to_codes/3,
	write_term_to_codes/3,
	write_term_to_codes/4,
%% [PM] 4.0 gone, use {atom,number}_codes instead.
%%	atom_to_codes/2,
%%	atom_to_codes/3,
%%	number_to_codes/2,
%%	number_to_codes/3,
	read_from_codes/2,
	read_term_from_codes/3,
	open_codes_stream/2,
	with_output_to_codes/2,
	with_output_to_codes/3,
	with_output_to_codes/4
		   ]).
library_module(fastrw, [
	fast_read/1, fast_read/2,
	fast_write/1, fast_write/2,
	fast_buf_read/2,
	fast_buf_write/3
   ]).
library_module(file_systems, [
        make_directory/1,                       % [PM] 4.0 OK
        %% Later: make_directory/2,

	close_all_streams/0,                    % [PM] 4.0 OK

	delete_file/1,                          % [PM] 4.0 OK
	delete_directory/1,                     % [PM] 4.0 OK
	delete_directory/2,                     % [PM] 4.0 OK

	directory_exists/1,                     % [PM] 4.0 OK
	directory_exists/2,                     % [PM] 4.0 OK
	directory_must_exist/1,                 % [PM] 4.0 OK
	directory_must_exist/2,                 % [PM] 4.0 OK

	file_exists/1,                          % [PM] 4.0 OK
	file_exists/2,                          % [PM] 4.0 OK
	file_must_exist/1,                      % [PM] 4.0 OK
	file_must_exist/2,                       % [PM] 4.0 OK

%% [PM] 4.0 Gone. If open/[3,4] does not suffice it should be fixed,
%%      not replaced by a library predicate.
%%	open_file/3,

	rename_file/2,                          % [PM] 4.0 OK
	rename_directory/2,                     % [PM] 4.0 OK (new)
	current_directory/1,                    % [PM] 4.0 OK
	current_directory/2,                    % [PM] 4.0 OK
	directory_member_of_directory/2,        % [PM] 4.0 OK
	directory_member_of_directory/3,        % [PM] 4.0 OK
	directory_member_of_directory/4,        % [PM] 4.0 OK
	directory_members_of_directory/1,       % [PM] 4.0 OK
	directory_members_of_directory/2,       % [PM] 4.0 OK
	directory_members_of_directory/3,       % [PM] 4.0 OK
	directory_property/2,                   % [PM] 4.0 OK
	directory_property/3,                   % [PM] 4.0 OK
	file_member_of_directory/2,             % [PM] 4.0 OK
	file_member_of_directory/3,             % [PM] 4.0 OK
	file_member_of_directory/4,             % [PM] 4.0 OK
	file_members_of_directory/1,            % [PM] 4.0 OK
	file_members_of_directory/2,            % [PM] 4.0 OK
	file_members_of_directory/3,            % [PM] 4.0 OK
	file_property/2,                        % [PM] 4.0 OK
	file_property/3                         % [PM] 4.0 OK

	% library(filename) has NOT been merged in
   ]).
library_module(heaps, [
	add_to_heap/4,		%   Heap x Key x Datum -> Heap
	empty_heap/1,		%   -> Heap
	delete_from_heap/4,	%   Heap -> Key x Datum x Heap
	get_from_heap/4,	%   Heap -> Key x Datum x Heap
	heap_size/2,		%   Heap -> Size
	heap_to_list/2,		%   Heap -> List
	is_heap/1,		%   Heap ->
	list_to_heap/2,		%   List -> Heap
	min_of_heap/3,		%   Heap -> Key x Datum
	min_of_heap/5,		%   Heap -> (Key x Datum) x (Key x Datum)
	portray_heap/1		%   Heap ->
   ]).
library_module(lists, [
	% library(sets), should have been library(basic)
	select/3,		%  Elem <- Set -> Set
	selectchk/3,		%  Elem x Set -> Set
	% library(lists)
	append/2,			%   ListOfLists -> List
	append/5,			%   List x List x List x List x List
	correspond/4,			%   Elem <- List x List -> Elem
	delete/3,			%   List x Elem -> List
	delete/4,			%   List x Elem x Count -> List
	is_list/1,			%   List ->
	keys_and_values/3,		%   KeyValList -> KeyList x ValList
	last/2,				%   List -> Elem
	nextto/3,			%   Elem, Elem <- List
	nth0/3,				%   Integer x List -> Elem
	nth0/4,				%   Integer x List -> Elem x List
	nth1/3,				%   Integer x List -> Elem
	nth1/4,				%   Integer x List -> Elem x List
	one_longer/2,			%   List x List ->
	perm/2,				%   List -> List
	perm2/4,			%   Elem x Elem -> Elem x Elem
	permutation/2,			%   List <-> List
	proper_length/2,		%   List -> Length
	remove_dups/2,			%   List -> Set
	rev/2,				%   List -> List
	reverse/2,			%   List -> List
	same_length/2,			%   List x List ->
	same_length/3,			%   List x List x Integer ->
	select/4,			%   Elem x List x Elem -> List
	selectchk/4,			%   Elem x List x Elem -> List
	shorter_list/2,			%   List x List ->
	subseq/3,			%   List -> List x List
	subseq0/2,			%   List -> List
	subseq1/2,			%   List -> List
	sumlist/2,			%   List -> Integer
	transpose/2,			%   ListOfLists <-> ListOfLists
	% library(length)
	append_length/3,			% List x List x Length
	append_length/4,			% List x List x List x Length
	prefix_length/3,			% Whole x Part x Length
	proper_prefix_length/3,			% Whole x Part x Length
	proper_suffix_length/3,			% Whole x Part x Length
	rotate_list/2,				% List x List
	rotate_list/3,				% Integer -> (List x List)
	suffix_length/3,			% Whole x Part x Length
	sublist/3,				% Whole x Part x Length
	sublist/4,				% Whole x Part x Length^2
	sublist/5,				% Whole x Part x Length^3
	% library(list_parts)
	cons/3,			last/3,
	head/2,			tail/2,
	prefix/2,		proper_prefix/2,
	suffix/2,		proper_suffix/2,
	segment/2,		proper_segment/2,
	% library(maplist)
	cumlist/4,
	cumlist/5,
	cumlist/6,
	maplist/2,
	maplist/3,
	maplist/4,
	map_product/4,
	scanlist/4,
	scanlist/5,
	scanlist/6,
	some/2,
	some/3,
	some/4,
	somechk/2,
	somechk/3,
	somechk/4,
	% library(more_maps)
	convlist/3,
	exclude/3,
	exclude/4,
	exclude/5,
	include/3,
	include/4,
	include/5,
	partition/5,
	group/3,
	group/4,
	group/5,
	% library(ordered)
	ordered/1,
	ordered/2,
	max_member/2,
	min_member/2,
	max_member/3,
	min_member/3,
	select_max/3,
	select_max/4,
	select_min/3,
	select_min/4,
	% library(ordprefix)
	decreasing_prefix/3,
	decreasing_prefix/4,
	increasing_prefix/3,
	increasing_prefix/4,
	% library(clump)
	clumped/2,
	clumps/2,
	keyclumped/2,
	keyclumps/2
   ]).
library_module(ordsets, [
	is_ordset/1,		%  Set ->
	list_to_ord_set/2,	%  List -> Set
	ord_add_element/3,	%  Set x Elem -> Set
	ord_del_element/3,	%  Set x Elem -> Set
	ord_disjoint/2,		%  Set x Set ->
	ord_disjoint_union/3,	%  Set x Set -> Set
	ord_intersect/2,	%  Set x Set ->
	ord_intersection/3,	%  Set x Set -> Set
	ord_intersection/4,	%  Set x Set -> Set x Set
	ord_intersection/2,	%  list(Set) -> Set
	ord_member/2,		%  Elem x Set ->
	ord_nonmember/2,	%  Elem x Set ->
	ord_seteq/2,		%  Set x Set ->
	ord_setproduct/3,	%  Set x Set -> SetOfPairs
	ord_subset/2,		%  Set x Set ->
	ord_subtract/3,		%  Set x Set -> Set
	ord_symdiff/3,		%  Set x Set -> Set
	ord_union/3,		%  Set x Set -> Set
	ord_union/2,		%  list(Set) -> Set
	ord_union/4,		%  Set x Set -> Set x Set
	ordset_order/3		%  Term x Term -> Relation
   ]).
library_module(plunit, [
	  begin_tests/1,	% +Name
	  begin_tests/2,	% +Name, +Options
	  end_tests/1,		% +Name
	  run_tests/0,		% 
	  run_tests/1,		% +Specs
	  run_tests/2		% +Specs, +Options
	  % set_test_options/1	% SWI specific
	  % load_test_files/1	% SWI specific
	  % running_tests/0	% SWI specific
	  % test_report/1	% SWI specific
	  ]).
library_module(process,
          [
           process_create/2,
           process_create/3,
           process_wait/2,
           process_wait/3,
           process_id/1,
           process_id/2,
           is_process/1,
           process_release/1,
           process_kill/1,
           process_kill/2
          ]).
library_module(queues, [
	append_queue/3,		% List x Queue -> Queue
	empty_queue/1,		% -> Queue
	singleton_queue/2,	% Item -> Queue
	is_queue/1,		% Queue ->
	list_queue/2,		% List <-> Queue
	portray_queue/1,	% Queue ->
	queue_append/3,		% Queue x List -> Queue
	queue_cons/3,		% Item x Queue -> Queue
	queue_head/2,		% Queue -> Item
	queue_last/2,		% Queue -> Queue
	queue_last/3,		% Queue x Item -> Queue
	queue_length/2,		% Queue <-> Integer
	queue_list/2,		% Queue <-> List
	queue_member/2,		% Item x Queue
	queue_memberchk/2,	% Item x Queue ->
	queue_tail/2,		% Queue -> Item
	map_queue/2,		% Pred x Queue ->
	map_queue/3,		% Pred x Queue x Queue
	map_queue_list/3,	% Pred x Queue x List
	map_list_queue/3,	% Pred x List x Queue
	some_queue/2,		% Pred x Queue
	some_queue/3,		% Pred x Queue x Queue
	somechk_queue/2,	% Pred x Queue ->
	somechk_queue/3		% Pred x Queue x Queue ->
   ]).
library_module(random, [
	getrand/1,			% save the state
	setrand/1,			% reset the state
	maybe/0,			% random success/failure
	maybe/1,			% random success/failure
	maybe/2,			% random success/failure
	random/1,			% uniform [0,1)
	random/3,			% uniform [L,U)
	random_numlist/4,		% Prob x Low x High -> Set
	random_member/2,		% Elem <- List
	random_select/3,		% Elem <- List -> Rest
	random_subseq/3,		% List -> Sbsq x Cmpl
	random_permutation/2,		% List -> Perm
	random_perm2/4			% Item x Item -> Item x Item
   ]).
library_module(rem, [
	rem_create/2,		% NumberOfNodes -> F
	rem_head/3,
	rem_add_link/4,	% Node x Node x F -> F
	rem_equivalent/3	% Node x Node x F ->
   ]).
library_module(samsort, [
	keymerge/3,
	merge/3,
	merge/4,
	samkeysort/2,
	samsort/2,
	samsort/3
   ]).
library_module(sets, [
	add_element/3,		%  Elem x Set -> Set
	del_element/3,		%  Elem x Set -> Set
	is_set/1,		%  List ->
	disjoint/2,		%  Set x Set ->
	disjoint_union/3,	%  Set x Set -> Set
	intersect/2,		%  Set x Set ->
	intersection/2,		%  list(Set) -> Set
	intersection/3,		%  Set x Set -> Set
	list_to_set/2,		%  List -> Set
	pairfrom/4,		%  Set -> Elem x Elem x Set
	power_set/2,		%  Set -> Set of Sets
	set_order/3,		%  Set x Set -> Relation
	seteq/2,		%  Set x Set ->
	setproduct/3,		%  Set x Set -> Pairs
	subset/2,		%  Set x Set ->
	subtract/3,		%  Set x Set -> Set
	symdiff/3,		%  Set x Set -> Set
	union/2,		%  list(Set) -> Set
	union/3,		%  Set x Set -> Set
	union/4			%  Set x Set -> Set x Set
   ]).
library_module(sockets, [
	current_host/1,

%% [PM] 4.0 Should be replaced with non-determinate hostname_address/3
%%	hostname_address/2

	socket_client_open/3,
	socket_server_open/2,
	socket_server_open/3,
        socket_server_close/1,
	socket_server_accept/4,
	socket_select/7

		   ]).
library_module(system, [
	environ/2,
	environ/3, % [PM] 4.1
	now/1,
	datime/1,
	datime/2,
	sleep/1]).
library_module(system3, [
        now/1,
        datime/1,
        datime/2,
        environ/2,
        sleep/1,
        host_name/1,
        host_id/1,
        file_exists/1,
        file_exists/2,
        file_property/2,
        rename_file/2,
        delete_file/1,
        delete_file/2,
        make_directory/1,
        working_directory/2,
        directory_files/2,
        mktemp/2,
        tmpnam/1,
        system/0,
        system/1,
        system/2,
        shell/0,
        shell/1,
        shell/2,
        exec/3,
        popen/3,
        pid/1,
        kill/2,
        wait/2
        ]).
library_module(tcltk, [
	tcl_new/1,
	tcl_delete/1,
	tcl_eval/3,
	tcl_event/3,
	tk_new/2,
	tk_main_window/2,
	tk_destroy_window/1,
	tk_make_window_exist/1,
	tk_num_main_windows/1,
	tk_do_one_event/0,
	tk_do_one_event/1,
	tk_next_event/2,
	tk_next_event/3,
	tk_main_loop/0,
	tk_terminal/5
	       ]).
library_module(terms, [
	subsumeschk/2, 
	subsumes/2, 
	variant/2, 
	term_subsumer/3, 
	term_hash/2,
	term_hash/3,		% [PM] 4.0.5
	term_hash/4,
        % [PM] 4.3 term_variables/2,
        term_variables_set/2,   % [PM] 4.3
	term_variables_bag/2,
	% [PM] 4.3 Now built-in (ISO Cor.2): acyclic_term/1,
	cyclic_term/1,
	% library(order)
	term_order/3,
	% library(occurs)
	contains_term/2,	%   T2 contains term T1
	contains_var/2,		%   T2 contains variable V1
	free_of_term/2,		%   T2 is free of term T1
	free_of_var/2,		%   T2 is free of variable V1
	occurrences_of_term/3,	%   T2 contains N3 instances of term T1
	occurrences_of_var/3,	%   T2 contains N3 instances of var V1
	sub_term/2,		%   T1 is a sub-term of T2 (enumerate T1)
	% library(term_depth)
	depth_bound/2,
	length_bound/2,
	size_bound/2,
	term_depth/2,
	term_size/2,
	% library(same_functor)
	same_functor/2,
	same_functor/3,
	same_functor/4
	]).
library_module(timeout, [time_out/3]).
library_module(trees, [
	gen_label/3,
	get_label/3,
	list_to_tree/2,
	map_tree/3,
	put_label/4,
	put_label/5,
	tree_size/2,
	tree_to_list/2
   ]).
library_module(ugraphs, [
	vertices_edges_to_ugraph/3,
	vertices/2,
	edges/2,
	add_vertices/3,
	del_vertices/3,
	add_edges/3,
	del_edges/3,
	transpose_ugraph/2,
	neighbors/3,
	neighbours/3,
	complement/2,
	compose/3,
	transitive_closure/2,
	symmetric_closure/2,
	top_sort/2,
	max_path/5,
	min_path/5,
	min_paths/3,
	path/3,
	reduce/2,
	reachable/3,
	random_ugraph/3,
	min_tree/3
   ]).
library_module(varnumbers, [
	numbervars/1,
	varnumbers/2,
	varnumbers/3
   ]).
library_module(wgraphs, [
	wgraph_to_ugraph/2,
	ugraph_to_wgraph/2,
	vertices_edges_to_wgraph/3,
	vertices/2,
	edges/2,
	add_vertices/3,
	del_vertices/3,
	add_edges/3,
	del_edges/3,
	transpose_wgraph/2,
	neighbors/3,
	neighbours/3,
	transitive_closure/2,
	symmetric_closure/2,
	top_sort/2,
	max_path/5,
	min_path/5,
	min_paths/3,
	path/3,
	reduce/2,
	reachable/3,
	random_wgraph/4,
	min_tree/3
   ]).
library_module( xml, [
	xml_parse/2,
	xml_parse/3,
	xml_subterm/2,
	xml_pp/1
	]).
