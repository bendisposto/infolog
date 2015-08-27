:- module(library_modules,[precompile_library_modules/0,
     library_export/2, export_list_available/1,
     is_library_module/1,
     library_module/2 ]).

:- dynamic library_export/2.
:- dynamic export_list_available/1.

library_export(_,_) :- print('**** LIBRARY EXPORT NOT PRECOMPILED ***'),nl,nl,fail.

precompile_library_modules :- retractall(library_export(_,_)),
   retractall(export_list_available(_)),
   library_module(M,List),
   assert(export_list_available(M)),
   member(P,List),
   assert(library_export(M,P)),fail.
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
is_library_module(process).
is_library_module(random).
is_library_module(samsort).
is_library_module(sets).
is_library_module(system).
is_library_module(tcltk).
is_library_module(terms).
is_library_module(timeout).
is_library_module(xml).

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
library_module(file_systems, [
        make_directory/1,                       % [PM] 4.0 OK
        %% Later: make_directory/2,

%% FIXME FIXME: I ([PM]) have reinstated some of the access-control preds
%% [PM] 4.0 It is not possible to reliably and portably mimic the OS
%%      access control checks (e.g., due to UNIX ACLs, Win32
%%      permissions, other processes accessing the file system).
%%      See for instance the discussion in SUSv3/POSIX on why
%%      eaccess() is not part of the standard (and why access(2) does
%%      not help, in general).
%%
%%      For this reason all predicates that does access control
%%      (except simple existence checks) have been removed until
%%      further notice.
%%
%%	can_open_file/2,
%%	can_open_file/3,

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
library_module(samsort, [
	keymerge/3,
	merge/3,
	merge/4,
	samkeysort/2,
	samsort/2,
	samsort/3
   ]).
