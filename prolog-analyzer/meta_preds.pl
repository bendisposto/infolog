:- module(meta_preds,[meta_library_pred/3]).

% module lists

meta_library_pred(cumlist(_,_,_,_), lists, [meta_arg(1,3)]).
meta_library_pred(cumlist(_,_,_,_,_), lists, [meta_arg(1,4)]).
meta_library_pred(cumlist(_,_,_,_,_,_), lists, [meta_arg(1,5)]).
meta_library_pred(maplist(_,_), lists, [meta_arg(1,1)]).
meta_library_pred(maplist(_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(maplist(_,_,_,_), lists, [meta_arg(1,3)]).
meta_library_pred(map_product(_,_,_,_), lists, [meta_arg(1,3)]).
meta_library_pred(scanlist(_,_,_,_), lists, [meta_arg(1,3)]).
meta_library_pred(scanlist(_,_,_,_,_), lists, [meta_arg(1,4)]).
meta_library_pred(scanlist(_,_,_,_,_,_), lists, [meta_arg(1,5)]).
meta_library_pred(some(_,_), lists, [meta_arg(1,1)]).
meta_library_pred(some(_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(some(_,_,_,_), lists, [meta_arg(1,3)]).
meta_library_pred(somechk(_,_), lists, [meta_arg(1,1)]).
meta_library_pred(somechk(_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(somechk(_,_,_,_), lists, [meta_arg(1,3)]).
meta_library_pred(convlist(_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(exclude(_,_,_), lists, [meta_arg(1,1)]).
meta_library_pred(exclude(_,_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(exclude(_,_,_,_,_), lists, [meta_arg(1,3)]).
meta_library_pred(include(_,_,_), lists, [meta_arg(1,1)]).
meta_library_pred(include(_,_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(include(_,_,_,_,_), lists, [meta_arg(1,3)]).
meta_library_pred(partition(_,_,_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(partition_1(_,_,_,_,_), lists, [meta_arg(2,2)]).
meta_library_pred(partition_1(_,_,_,_,_,_,_), lists, [meta_arg(7,2)]).
meta_library_pred(group(_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(group1(_,_,_), lists, [meta_arg(3,2)]).
meta_library_pred(group(_,_,_,_), lists, [meta_arg(1,1)]).
meta_library_pred(group(_,_,_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(ordered(_,_), lists, [meta_arg(1,2)]).
meta_library_pred(ordered_0(_,_), lists, [meta_arg(2,2)]).
meta_library_pred(ordered_1(_,_,_), lists, [meta_arg(3,2)]).
meta_library_pred(max_member(_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(min_member(_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(select_min(_,_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(sel_min_gen(_,_,_,_), lists, [meta_arg(3,2)]).
meta_library_pred(sel_min_gen(_,_,_,_,_,_), lists, [meta_arg(4,2)]).
meta_library_pred(select_max(_,_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(sel_max_gen(_,_,_,_), lists, [meta_arg(3,2)]).
meta_library_pred(sel_max_gen(_,_,_,_,_,_), lists, [meta_arg(4,2)]).
meta_library_pred(decreasing_prefix(_,_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(first_precedes(_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(increasing_prefix(_,_,_,_), lists, [meta_arg(1,2)]).
meta_library_pred(precedes_first(_,_,_), lists, [meta_arg(1,2)]).

% module avl

meta_library_pred(avl_map(_,_), avl, [meta_arg(1,1)]).
meta_library_pred(avl_map(_,_,_), avl, [meta_arg(1,2)]).

% module samsort

meta_library_pred(merge(_,_,_,_), samsort, [meta_arg(1,2)]).
meta_library_pred(sam_merge(_,_,_,_), samsort, [meta_arg(3,2)]).
meta_library_pred(samsort(_,_,_), samsort, [meta_arg(1,2)]).
meta_library_pred(sam_sort(_,_,_,_,_), samsort, [meta_arg(2,2)]).
meta_library_pred(sam_run(_,_,_,_,_,_), samsort, [meta_arg(4,2)]).
meta_library_pred(sam_rest(_,_,_,_,_,_,_), samsort, [meta_arg(5,2)]).
meta_library_pred(sam_fuse(_,_,_), samsort, [meta_arg(2,2)]).
meta_library_pred(sam_fuse(_,_,_,_,_), samsort, [meta_arg(3,2)]).

% module assoc

meta_library_pred(map_assoc(_,_), assoc, [meta_arg(1,1)]).
meta_library_pred(map_assoc(_,_,_), assoc, [meta_arg(1,2)]).

% module queues

meta_library_pred(map_queue(_,_), queues, [meta_arg(1,1)]).
meta_library_pred(map_queue(_,_,_), queues, [meta_arg(1,2)]).
meta_library_pred(map_queue_list(_,_,_), queues, [meta_arg(1,2)]).
meta_library_pred(map_list_queue(_,_,_), queues, [meta_arg(1,2)]).
meta_library_pred(some_queue(_,_), queues, [meta_arg(1,1)]).
meta_library_pred(some_queue(_,_,_), queues, [meta_arg(1,2)]).
meta_library_pred(somechk_queue(_,_), queues, [meta_arg(1,1)]).
meta_library_pred(somechk_queue(_,_,_), queues, [meta_arg(1,2)]).

% module bags

meta_library_pred(checkbag(_,_), bags, [meta_arg(1,2)]).
meta_library_pred(check_bag(_,_), bags, [meta_arg(2,2)]).
meta_library_pred(mapbag(_,_), bags, [meta_arg(1,1)]).
meta_library_pred(map_bag(_,_), bags, [meta_arg(2,1)]).
meta_library_pred(mapbag(_,_,_), bags, [meta_arg(1,2)]).
meta_library_pred(map_bag_list(_,_,_), bags, [meta_arg(3,2)]).
meta_library_pred(somebag(_,_), bags, [meta_arg(1,2)]).
meta_library_pred(somechkbag(_,_), bags, [meta_arg(1,2)]).

% module trees

meta_library_pred(map_tree(_,_,_), trees, [meta_arg(1,2)]).
meta_library_pred('map tree'(_,_,_), trees, [meta_arg(3,2)]).

