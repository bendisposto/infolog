meta_user_pred(module_info(_,_), module_information, [meta_arg(1,0)]).

meta_user_pred(assert_pre(_,_), self_check, [meta_arg(1,0),meta_arg(2,0)]).

meta_user_pred(assert_post(_,_), self_check, [meta_arg(1,0),meta_arg(2,0)]).

meta_user_pred(assert_must_succeed_any(_), self_check, [meta_arg(1,0)]).

meta_user_pred(assert_must_succeed(_), self_check, [meta_arg(1,0)]).

meta_user_pred(assert_must_succeed_multiple(_), self_check, [meta_arg(1,0)]).

meta_user_pred(assert_must_fail(_), self_check, [meta_arg(1,0)]).

meta_user_pred(catch_call(_), tools, [meta_arg(1,0)]).

meta_user_pred(safe_on_exception(_,_,_), tools, [meta_arg(2,0),meta_arg(3,0)]).

meta_user_pred(catch_matching(_,_,_), tools, [meta_arg(1,0),meta_arg(3,0)]).

meta_user_pred('~~'(_), debug, [meta_arg(1,0)]).

meta_user_pred(timer_call(_), debug, [meta_arg(1,0)]).

meta_user_pred(timer_call(_,_), debug, [meta_arg(2,0)]).

meta_user_pred(time_if_debug(_), debug, [meta_arg(1,0)]).

meta_user_pred(time(_), debug, [meta_arg(1,0)]).

meta_user_pred(watch_det(_,_), debug, [meta_arg(2,0)]).

meta_user_pred(watch(_), debug, [meta_arg(1,0)]).

meta_user_pred(watch(_,_), debug, [meta_arg(2,0)]).

meta_user_pred(det_check(_), debug, [meta_arg(1,0)]).

meta_user_pred(det_check(_,_), debug, [meta_arg(1,0),meta_arg(2,0)]).

meta_user_pred(if_det_check(_,_,_), debug, [meta_arg(1,0),meta_arg(2,0),meta_arg(3,0)]).

meta_user_pred(if_det_check_pp(_,_,_,_), debug, [meta_arg(1,0),meta_arg(2,0),meta_arg(3,0)]).

meta_user_pred(add_failed_call_error(_), error_manager, [meta_arg(1,0)]).

meta_user_pred(add_internal_error(_,_), error_manager, [meta_arg(2,0)]).

meta_user_pred(call_in_fresh_error_scope_for_one_solution(_), error_manager, [meta_arg(1,0)]).

meta_user_pred(add_new_event_in_error_scope(_,_), error_manager, [meta_arg(2,1)]).

meta_user_pred(on_enumeration_warning(_,_), error_manager, [meta_arg(1,0),meta_arg(2,0)]).

meta_user_pred(on_enumeration_warning_with_continue(_,_,_), error_manager, [meta_arg(1,0),meta_arg(2,0),meta_arg(3,0)]).

meta_user_pred(catch_enumeration_warning_exceptions(_,_), error_manager, [meta_arg(1,0),meta_arg(2,0)]).

meta_user_pred(catch_enumeration_warning_exceptions(_,_,_), error_manager, [meta_arg(1,0),meta_arg(2,0)]).

meta_user_pred(call_with_enumeration_warning(_), error_manager, [meta_arg(1,0)]).

meta_user_pred(time_out_with_enum_warning_one_solution(_,_,_), error_manager, [meta_arg(1,0)]).

meta_user_pred(time_out_with_enum_warning_one_solution_no_new_error_scope(_,_,_), error_manager, [meta_arg(1,0)]).

meta_user_pred(time_out_with_enum_warning_for_findall(_,_,_), error_manager, [meta_arg(1,0)]).

meta_user_pred(time_out_with_enum_warning_for_findall2(_,_,_,_), error_manager, [meta_arg(2,0)]).

meta_user_pred(time_out_with_enum_warning_for_findall_in_current_error_scope(_,_,_,_), error_manager, [meta_arg(2,0)]).

meta_user_pred(filter(_,_,_,_), tools, [meta_arg(1,1)]).

meta_user_pred(get_options(_,_,_,_), tools, [meta_arg(2,4)]).

meta_user_pred(get_options(_,_,_,_,_), tools, [meta_arg(2,4),meta_arg(5,0)]).

meta_user_pred(call_residue(_,_), tools, [meta_arg(1,0)]).

meta_user_pred(space_call(_), tools, [meta_arg(1,0)]).

meta_user_pred(split_list(_,_,_,_), tools, [meta_arg(1,1)]).

meta_user_pred(split_list2(_,_,_,_), tools, [meta_arg(2,1)]).

meta_user_pred(foldl(_,_,_,_), tools, [meta_arg(1,3)]).

meta_user_pred(foldl(_,_,_,_,_), tools, [meta_arg(1,4)]).

meta_user_pred(foldl(_,_,_,_,_,_), tools, [meta_arg(1,5)]).

meta_user_pred(assert_once(_), tools, [meta_arg(1,0)]).

meta_user_pred(safe_time_out(_,_,_), tools, [meta_arg(1,0)]).

meta_user_pred(call_with_preference(_,_,_), preferences, [meta_arg(1,0)]).

meta_user_pred(register_event_listener(_,_,_), eventhandling, [meta_arg(2,0)]).

meta_user_pred(time_out_with_factor_call(_,_,_), self_check, [meta_arg(1,0),meta_arg(3,0)]).

meta_user_pred(time_out_call(_,_), self_check, [meta_arg(1,0),meta_arg(2,0)]).

meta_user_pred(time_out_call(_), self_check, [meta_arg(1,0)]).

meta_user_pred(pp_mnf(_), self_check, [meta_arg(1,0)]).

meta_user_pred(pp_cll(_), self_check, [meta_arg(1,0)]).

meta_user_pred(mnf(_), self_check, [meta_arg(1,0)]).

meta_user_pred(mnf(_,_), self_check, [meta_arg(2,0)]).

meta_user_pred(det_call(_), self_check, [meta_arg(1,0)]).

meta_user_pred(residue_check_call(_), self_check, []).

meta_user_pred(must_fail(_), self_check, [meta_arg(1,0)]).

meta_user_pred(must_succeed(_), self_check, [meta_arg(1,0)]).

meta_user_pred(must_succeed_without_residue(_), self_check, [meta_arg(1,0)]).

meta_user_pred(must_succeed_multiple_without_residue(_), self_check, [meta_arg(1,0)]).

meta_user_pred(safe_call(_), self_check, [meta_arg(1,0)]).

meta_user_pred(check_deterministic(_), self_check, [meta_arg(1,0)]).

meta_user_pred(check_det(_), self_check, [meta_arg(1,0)]).

meta_user_pred(assert_must_abort_wf(_,_), kernel_waitflags, [meta_arg(1,0)]).

meta_user_pred(post_constraint(_), clpfd_interface, [meta_arg(1,0)]).

meta_user_pred(post_constraint(_,_), clpfd_interface, [meta_arg(1,0),meta_arg(2,0)]).

meta_user_pred(post_constraint2(_,_), clpfd_interface, [meta_arg(1,0)]).

meta_user_pred(time_out_constraint(_,_), clpfd_interface, [meta_arg(1,0),meta_arg(2,0)]).

meta_user_pred(catch_clpfd_overflow_call1(_), clpfd_interface, [meta_arg(1,0)]).

meta_user_pred(catch_clpfd_overflow_call2(_,_), clpfd_interface, [meta_arg(1,0),meta_arg(2,0)]).

meta_user_pred(map_over_bvalue(_,_,_), kernel_tools, [meta_arg(1,2)]).

meta_user_pred(my_findall(_,_,_,_), delay, [meta_arg(2,0)]).

meta_user_pred(my_findall_catch(_,_,_,_), delay, [meta_arg(2,0)]).

meta_user_pred(my_findall_catch(_,_,_,_,_), delay, [meta_arg(2,0)]).

meta_user_pred(my_findall_check(_,_,_,_,_,_), delay, [meta_arg(2,0),meta_arg(5,0),meta_arg(6,0)]).

meta_user_pred(succeed_max_call(_,_), succeed_max, [meta_arg(1,0)]).

meta_user_pred(succeed_max_call_id(_,_,_), succeed_max, [meta_arg(2,0)]).

meta_user_pred(open_cache_file(_,_,_,_), value_persistance, [meta_arg(4,0)]).

meta_user_pred(show_cache_file_contents_for_machine(_,_,_,_), value_persistance, [meta_arg(2,0)]).

meta_user_pred(apply_transformation_step(_,_,_,_), bmachine, [meta_arg(2,2)]).

meta_user_pred(kernel_call_or(_,_,_,_,_), kernel_mappings, [meta_arg(1,0)]).

meta_user_pred(wd_delay(_,_,_,_), b_interpreter_check, [meta_arg(1,0)]).

meta_user_pred(wd_delay_block(_,_,_,_,_,_), b_interpreter_check, [meta_arg(1,0)]).

meta_user_pred(must_succ(_,_), kernel_mappings, [meta_arg(1,0)]).

meta_user_pred(delay_setof_with_explicit_waitvars(_,_,_,_), delay, [meta_arg(2,0)]).

meta_user_pred(delay_setof(_,_,_,_,_), delay, [meta_arg(2,0)]).

meta_user_pred(delay_setof_check(_,_,_,_,_,_,_), delay, [meta_arg(2,0),meta_arg(6,0),meta_arg(7,0)]).

meta_user_pred(delay_setof_list(_,_,_,_), delay, [meta_arg(2,0)]).

meta_user_pred(delay_call(_,_,_), delay, [meta_arg(1,0)]).

meta_user_pred(delay_call(_,_), delay, [meta_arg(1,0)]).

meta_user_pred(delay_not(_,_), delay, [meta_arg(1,0)]).

meta_user_pred(not_with_enum_warning(_), delay, [meta_arg(1,0)]).

meta_user_pred(profile_single_call(_,_,_), runtime_profiler, [meta_arg(3,0)]).

meta_user_pred(profile_single_call(_,_), runtime_profiler, [meta_arg(2,0)]).

meta_user_pred(call_with_smt_mode_enabled(_), solver_interface, [meta_arg(1,0)]).

meta_user_pred(call_with_chr(_), solver_interface, [meta_arg(1,0)]).

meta_user_pred(findall_keepvars(_,_,_), haskell_csp_analyzer, [meta_arg(2,0)]).

meta_user_pred(read_compiled_prolog_file(_,_,_), haskell_csp, [meta_arg(3,1)]).

meta_user_pred(map_over_bexpr(_,_), bsyntaxtree, [meta_arg(1,1)]).

meta_user_pred(map_over_typed_bexpr(_,_), bsyntaxtree, [meta_arg(1,1)]).

meta_user_pred(map_over_typed_bexpr(_,_,_), bsyntaxtree, [meta_arg(1,2)]).

meta_user_pred(map_over_bexpr_top_down_acc(_,_,_), bsyntaxtree, [meta_arg(1,2)]).

meta_user_pred(reduce_over_bexpr(_,_,_,_), bsyntaxtree, [meta_arg(1,3)]).

meta_user_pred(transform_bexpr(_,_,_), bsyntaxtree, [meta_arg(1,2)]).

meta_user_pred(transform_bexpr_with_acc(_,_,_,_,_), bsyntaxtree, [meta_arg(1,4)]).