require "llvm"

lib LibLLVM
  fun get_basic_block_parent = LLVMGetBasicBlockParent(bp : LibLLVM::BasicBlockRef) : ValueRef

  fun count_basic_blocks = LLVMCountBasicBlocks(fn : ValueRef) : UInt32

  fun build_cond_br = LLVMBuildCondBr(BuilderRef, ValueRef, BasicBlockRef, BasicBlockRef) : ValueRef

  fun build_f_neg = LLVMBuildFNeg(BuilderRef,ValueRef,UInt8* ): ValueRef

  fun verify_function = LLVMVerifyFunction(ValueRef, LLVM::VerifierFailureAction): Int32
  fun run_function_as_main = LLVMRunFunctionAsMain(ee : ExecutionEngineRef, f : ValueRef, arg_c : LibC::UInt, arg_v : LibC::Char**, env_p : LibC::Char**) : LibC::Int

  fun add_aggressive_dce_pass = LLVMAddAggressiveDCEPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_cfg_simplification_pass = LLVMAddCFGSimplificationPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_dead_store_elimination_pass = LLVMAddDeadStoreEliminationPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_gvn_pass = LLVMAddGVNPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_ind_var_simplify_pass = LLVMAddIndVarSimplifyPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_instruction_combining_pass = LLVMAddInstructionCombiningPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_jump_threading_pass = LLVMAddJumpThreadingPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_licm_pass = LLVMAddLICMPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_loop_deletion_pass = LLVMAddLoopDeletionPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_loop_idiom_pass = LLVMAddLoopIdiomPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_loop_rotate_pass = LLVMAddLoopRotatePass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_loop_reroll_pass = LLVMAddLoopRerollPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_loop_unroll_pass = LLVMAddLoopUnrollPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_loop_unswitch_pass = LLVMAddLoopUnswitchPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_mem_cpy_opt_pass = LLVMAddMemCpyOptPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_partially_inline_lib_calls_pass = LLVMAddPartiallyInlineLibCallsPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_promote_memory_to_register_pass = LLVMAddPromoteMemoryToRegisterPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_reassociate_pass = LLVMAddReassociatePass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_sccp_pass = LLVMAddSCCPPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_scalar_repl_aggregates_pass = LLVMAddScalarReplAggregatesPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_scalar_repl_aggregates_pass_ssa = LLVMAddScalarReplAggregatesPassSSA(pm : LibLLVM::PassManagerRef) : Void*
  fun add_scalar_repl_aggregates_pass_with_threshold = LLVMAddScalarReplAggregatesPassWithThreshold(pm : LibLLVM::PassManagerRef) : Void*
  fun add_simplify_lib_calls_pass = LLVMAddSimplifyLibCallsPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_tail_call_elimination_pass = LLVMAddTailCallEliminationPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_constant_propagation_pass = LLVMAddConstantPropagationPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_demote_memory_to_register_pass = LLVMAddDemoteMemoryToRegisterPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_verifier_pass = LLVMAddVerifierPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_correlated_value_propagation_pass = LLVMAddCorrelatedValuePropagationPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_early_cse_pass = LLVMAddEarlyCSEPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_lower_expect_intrinsic_pass = LLVMAddLowerExpectIntrinsicPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_type_based_alias_analysis_pass = LLVMAddTypeBasedAliasAnalysisPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_basic_alias_analysis_pass = LLVMAddBasicAliasAnalysisPass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_bb_vectorize_pass = LLVMAddBBVectorizePass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_loop_vectorize_pass = LLVMAddLoopVectorizePass(pm : LibLLVM::PassManagerRef) : Void*
  fun add_slp_vectorize_pass = LLVMAddSLPVectorizePass(pm : LibLLVM::PassManagerRef) : Void*
end
