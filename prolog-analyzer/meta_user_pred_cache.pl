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

meta_user_pred(call_residue(_,_), tools_meta, [meta_arg(1,0)]).

meta_user_pred(space_call(_), tools, [meta_arg(1,0)]).

meta_user_pred(split_list(_,_,_,_), tools, [meta_arg(1,1)]).

meta_user_pred(split_list2(_,_,_,_), tools, [meta_arg(2,1)]).

meta_user_pred(foldl(_,_,_,_), tools, [meta_arg(1,3)]).

meta_user_pred(foldl(_,_,_,_,_), tools, [meta_arg(1,4)]).

meta_user_pred(foldl(_,_,_,_,_,_), tools, [meta_arg(1,5)]).

meta_user_pred(assert_once(_), tools, [meta_arg(1,0)]).

meta_user_pred(call_with_preference(_,_,_), preferences, [meta_arg(1,0)]).

meta_user_pred(register_event_listener(_,_,_), eventhandling, [meta_arg(2,0)]).

meta_user_pred(pp_mnf(_), self_check, [meta_arg(1,0)]).

meta_user_pred(pp_cll(_), self_check, [meta_arg(1,0)]).

meta_user_pred(mnf(_), self_check, [meta_arg(1,0)]).

meta_user_pred(mnf(_,_), self_check, [meta_arg(2,0)]).

meta_user_pred(det_call(_), self_check, [meta_arg(1,0)]).

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

meta_user_pred(map_over_bexpr_top_down_acc(_,_,_), bsyntaxtree, [meta_arg(1,3)]).

meta_user_pred(reduce_over_bexpr(_,_,_,_), bsyntaxtree, [meta_arg(1,3)]).

meta_user_pred(transform_bexpr(_,_,_), bsyntaxtree, [meta_arg(1,2)]).

meta_user_pred(transform_bexpr_with_acc(_,_,_,_,_), bsyntaxtree, [meta_arg(1,4)]).

meta_user_pred(user_interruptable_call_det(_,_), user_signal, [meta_arg(1,0)]).

meta_user_pred(protect_from_user_interrupt_det(_), user_signal, [meta_arg(1,0)]).

meta_user_pred(ignore_user_interrupt_det(_), user_signal, [meta_arg(1,0)]).

meta_user_pred(catch_interrupt_exception(_,_), user_signal, [meta_arg(1,0)]).

meta_user_pred(c_ltl_modelcheck(_,_,_,_), ltlc, [meta_arg(4,5)]).

meta_user_pred(evaluate_ltl_formula(_,_,_,_,_,_), ltl_verification, [meta_arg(4,2),meta_arg(5,3)]).

meta_user_pred(evaluate_ltl_fairness(_,_,_,_,_), ltl_verification, [meta_arg(4,3)]).

meta_user_pred(bv_time_out_call(_,_,_,_), bvisual2, [meta_arg(1,0)]).

meta_user_pred(call_with_temp_preference(_,_,_), bvisual2, [meta_arg(3,0)]).

meta_user_pred(probcli_clpfd_overflow_mnf_call1(_), eval_strings, [meta_arg(1,0)]).

meta_user_pred(probcli_clpfd_overflow_call1(_), eval_strings, [meta_arg(1,0)]).

meta_user_pred(maxsolver(_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(maxsolver(_,_,_,_), maxsolver, [meta_arg(2,1)]).

meta_user_pred(maxsolver_by_longest_prefix(_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(x_append_and_eval(_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(x_prepend_and_eval(_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(longest_satisfiable_prefix(_,_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(x_longest_satisfiable_prefix(_,_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(satisfiable_segment(_,_,_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(longest_satisfiable_segment(_,_,_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(maxsolver_by_longest_segment(_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(x_maxsolver_by_longest_segment(_,_,_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(maxsolver_exact_with_marker(_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(x_precalc(_,_,_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(visit_tree(_,_,_,_,_), bvisual, [meta_arg(1,3),meta_arg(2,2)]).

meta_user_pred(reduce_state_space(_,_), state_space_reduction, [meta_arg(1,3),meta_arg(2,5)]).

meta_user_pred(catch_and_ignore_well_definedness_error(_), static_analysis, [meta_arg(1,0)]).

meta_user_pred(printtime(_), ctl, [meta_arg(1,0)]).

meta_user_pred(register_conjunct_error_hook(_), predicate_evaluator, [meta_arg(1,5)]).

meta_user_pred(analyse_quick_time_out(_), predicate_evaluator, [meta_arg(1,0)]).

meta_user_pred(time_out_and_catch_errors(_,_,_), predicate_evaluator, [meta_arg(1,0)]).

meta_user_pred(catch_interrupt_assertion_call(_), user_interrupts, [meta_arg(1,0)]).

meta_user_pred(interruptable_call(_), user_interrupts, [meta_arg(1,0)]).

meta_user_pred(interruptable_call(_,_), user_interrupts, [meta_arg(1,0)]).

meta_user_pred(l_transform_bexpr(_,_,_), bsyntaxtree, [meta_arg(2,2)]).

meta_user_pred(l_transform_bexpr_with_acc(_,_,_,_,_), bsyntaxtree, [meta_arg(2,4)]).

meta_user_pred(foldl2(_,_,_,_), tools, [meta_arg(2,3)]).

meta_user_pred(foldl2(_,_,_,_,_), tools, [meta_arg(2,4)]).

meta_user_pred(foldl2(_,_,_,_,_,_), tools, [meta_arg(2,5)]).

meta_user_pred(x_false_subset(_,_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(x_satisfiable_subset_of_length(_,_,_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(x_false_subset_til_true(_,_,_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(x_existis_satisfiable_subset_of_length(_,_,_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(x_maxsolver_exact_with_marker(_,_,_,_,_,_), maxsolver, [meta_arg(1,1)]).

meta_user_pred(call_pred_on_expanded_state(_,_,_,_), user, [meta_arg(1,3)]).

meta_user_pred(map_over_history(_,_), user, [meta_arg(1,3)]).

meta_user_pred(time_out_with_factor_call(_,_,_), tools_timeout, [meta_arg(1,0),meta_arg(3,0)]).

meta_user_pred(time_out_call(_,_), tools_timeout, [meta_arg(1,0),meta_arg(2,0)]).

meta_user_pred(time_out_call(_), tools_timeout, [meta_arg(1,0)]).

meta_user_pred(safe_time_out(_,_,_), tools_meta, [meta_arg(1,0)]).

meta_user_pred(catch_clpfd_overflow_call_for_state(_,_,_), user, [meta_arg(2,0),meta_arg(3,0)]).

meta_user_pred(catch_interrupt_assertion_call(_), user, [meta_arg(1,0)]).

meta_user_pred(tcltk_time_call(_), user, [meta_arg(1,0)]).

meta_user_pred(add_csp_process_id1(_,_,_), user, [meta_arg(3,1)]).

meta_user_pred(block_my_findall_catch(_,_,_,_,_), delay, [meta_arg(3,0)]).

meta_user_pred(block_findall_check(_,_,_,_,_,_,_,_), delay, [meta_arg(3,0),meta_arg(5,0),meta_arg(6,0)]).

meta_user_pred(block_my_findall_sort(_,_,_,_), delay, [meta_arg(3,0)]).

meta_user_pred(not_with_enum_warning2(_,_), delay, [meta_arg(1,0)]).

meta_user_pred(find_enabled_fairids_for_state(_,_,_), ltl_verification, [meta_arg(1,3)]).

meta_user_pred(find_enabled_fairids(_,_,_,_), ltl_verification, [meta_arg(2,3)]).

meta_user_pred(eval_fairness(_,_,_,_), ltl_verification, [meta_arg(3,3)]).

meta_user_pred(is_executed(_,_,_), ltl_verification, [meta_arg(1,3)]).

meta_user_pred(is_executed2(_,_,_), ltl_verification, [meta_arg(1,3)]).

meta_user_pred(call_for_event(_,_), eventhandling, [meta_arg(2,0)]).

meta_user_pred(mnf_call(_), self_check, [meta_arg(1,0)]).

meta_user_pred(mnf_call_with_pp(_,_), self_check, [meta_arg(2,0)]).

meta_user_pred(prepost_mnf_call(_), self_check, [meta_arg(1,0)]).

meta_user_pred(prepost_call(_), self_check, [meta_arg(1,0)]).

meta_user_pred(check_exception_call(_), self_check, [meta_arg(1,0)]).

meta_user_pred(rt_timeout_call(_), self_check, [meta_arg(1,0)]).

meta_user_pred(mnf_det(_), self_check, [meta_arg(1,0)]).

meta_user_pred(residue_check_call(_), self_check, [meta_arg(1,0)]).

meta_user_pred(check_det2(_,_), self_check, [meta_arg(1,0)]).

meta_user_pred(safe_call(_,_), external_functions, [meta_arg(1,0)]).

meta_user_pred(try_call(_,_,_), kodkod, [meta_arg(3,0)]).

meta_user_pred(force_det_call(_), self_check, [meta_arg(1,0)]).