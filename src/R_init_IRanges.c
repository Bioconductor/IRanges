#include "IRanges.h"

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#define REGISTER_CCALLABLE(fun) \
	R_RegisterCCallable("IRanges", #fun, (DL_FUNC) &fun)

static const R_CallMethodDef callMethods[] = {

/* SEXP_utils.c */
	CALLMETHOD_DEF(sapply_NROW, 1),
        CALLMETHOD_DEF(top_prenv, 2),
        CALLMETHOD_DEF(top_prenv_dots, 1),

/* int_utils.c */
	CALLMETHOD_DEF(Integer_any_missing_or_outside, 3),
	CALLMETHOD_DEF(Integer_sum_non_neg_vals, 1),
	CALLMETHOD_DEF(Integer_diff_with_0, 1),
        CALLMETHOD_DEF(Integer_diff_with_last, 2),
	CALLMETHOD_DEF(Integer_order, 2),
	CALLMETHOD_DEF(Integer_order2, 3),
	CALLMETHOD_DEF(Integer_match2_quick, 5),
	CALLMETHOD_DEF(Integer_selfmatch2_quick, 2),
	CALLMETHOD_DEF(Integer_match2_hash, 5),
	CALLMETHOD_DEF(Integer_selfmatch2_hash, 2),
	CALLMETHOD_DEF(Integer_order4, 5),
	CALLMETHOD_DEF(Integer_match4_quick, 9),
	CALLMETHOD_DEF(Integer_selfmatch4_quick, 4),
	CALLMETHOD_DEF(Integer_match4_hash, 9),
	CALLMETHOD_DEF(Integer_selfmatch4_hash, 4),
	CALLMETHOD_DEF(Integer_tabulate2, 4),
	CALLMETHOD_DEF(Integer_explode_bits, 2),
	CALLMETHOD_DEF(Integer_sorted_merge, 2),
	CALLMETHOD_DEF(Integer_mseq, 2),
	CALLMETHOD_DEF(Integer_fancy_mseq, 3),
	CALLMETHOD_DEF(findIntervalAndStartFromWidth, 2),

/* str_utils.c */
	CALLMETHOD_DEF(unstrsplit_list, 2),
	CALLMETHOD_DEF(safe_strexplode, 1),
	CALLMETHOD_DEF(strsplit_as_list_of_ints, 2),
	CALLMETHOD_DEF(svn_time, 0),

/* compact_bitvector.c */
	CALLMETHOD_DEF(logical_as_compact_bitvector, 1),
	CALLMETHOD_DEF(compact_bitvector_as_logical, 2),
	CALLMETHOD_DEF(subset_compact_bitvector, 2),
	CALLMETHOD_DEF(compact_bitvector_bit_count, 1),
	CALLMETHOD_DEF(compact_bitvector_last_bit, 1),
	CALLMETHOD_DEF(compact_bitvector_set_op, 3),

/* Vector_class.c */
	CALLMETHOD_DEF(vector_subsetByRanges, 3),
	CALLMETHOD_DEF(vector_seqselect, 3),

/* Ranges_comparison.c */
	CALLMETHOD_DEF(Ranges_compare, 4),

/* IRanges_class.c */
	CALLMETHOD_DEF(debug_IRanges_class, 0),
	CALLMETHOD_DEF(IRanges_isNormal, 1),
	CALLMETHOD_DEF(IRanges_from_integer, 1),
	CALLMETHOD_DEF(NormalIRanges_from_logical, 1),

/* IRanges_constructor.c */
	CALLMETHOD_DEF(solve_user_SEW0, 3),
	CALLMETHOD_DEF(solve_user_SEW, 6),

/* Grouping_class.c */
	CALLMETHOD_DEF(debug_Grouping_class, 0),

	CALLMETHOD_DEF(H2LGrouping_members, 2),
	CALLMETHOD_DEF(H2LGrouping_vmembers, 2),

/* Rle_class.c */
	CALLMETHOD_DEF(Rle_constructor, 4),
	CALLMETHOD_DEF(Rle_start, 1),
	CALLMETHOD_DEF(Rle_end, 1),
	CALLMETHOD_DEF(Rle_getStartEndRunAndOffset, 3),
	CALLMETHOD_DEF(Rle_window_aslist, 5),
	CALLMETHOD_DEF(Rle_window, 6),
	CALLMETHOD_DEF(Rle_seqselect, 3),

/* Rle_utils.c */
	CALLMETHOD_DEF(Rle_runsum, 3),
	CALLMETHOD_DEF(Rle_runwtsum, 4),
	CALLMETHOD_DEF(Rle_runq, 4),

/* RleViews_utils.c */
	CALLMETHOD_DEF(RleViews_viewMins, 2),
	CALLMETHOD_DEF(RleViews_viewMaxs, 2),
	CALLMETHOD_DEF(RleViews_viewSums, 2),
	CALLMETHOD_DEF(RleViews_viewMeans, 2),
	CALLMETHOD_DEF(RleViews_viewWhichMins, 2),
	CALLMETHOD_DEF(RleViews_viewWhichMaxs, 2),

/* SimpleIRangesList_class.c */
	CALLMETHOD_DEF(SimpleIRangesList_isNormal, 1),
	CALLMETHOD_DEF(SimpleNormalIRangesList_min, 1),
	CALLMETHOD_DEF(SimpleNormalIRangesList_max, 1),

/* CompressedIRangesList_class.c */
	CALLMETHOD_DEF(CompressedIRangesList_isNormal, 2),
	CALLMETHOD_DEF(CompressedIRangesList_summary, 1),
	CALLMETHOD_DEF(CompressedNormalIRangesList_min, 2),
	CALLMETHOD_DEF(CompressedNormalIRangesList_max, 2),

/* GappedRanges_class.c */
	CALLMETHOD_DEF(valid_GappedRanges, 2),

/* Hits_class.c */
	CALLMETHOD_DEF(make_all_group_inner_hits, 2),

/* inter_range_methods.c */
	CALLMETHOD_DEF(debug_inter_range_methods, 0),
	CALLMETHOD_DEF(IRanges_range, 1),
	CALLMETHOD_DEF(Ranges_reduce, 6),
	CALLMETHOD_DEF(CompressedIRangesList_reduce, 4),
	CALLMETHOD_DEF(IRanges_gaps, 4),
	CALLMETHOD_DEF(CompressedIRangesList_gaps, 3),
	CALLMETHOD_DEF(Ranges_disjointBins, 2),

/* coverage_methods.c */
	CALLMETHOD_DEF(IRanges_coverage, 6),
	CALLMETHOD_DEF(CompressedIRangesList_coverage, 6),

	{NULL, NULL, 0}
};


void R_init_IRanges(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);

/* SEXP_utils.c */
	REGISTER_CCALLABLE(_get_classname);

/* int_utils.c */
	REGISTER_CCALLABLE(_check_integer_pairs);

/* Vector_class.c */
	REGISTER_CCALLABLE(_get_List_elementType);
	REGISTER_CCALLABLE(_set_List_elementType);
	REGISTER_CCALLABLE(_vector_memcmp);
	REGISTER_CCALLABLE(_vector_memcpy);

/* Ranges_comparison.c */
	REGISTER_CCALLABLE(_overlap_code);
	REGISTER_CCALLABLE(_invert_overlap_code);

/* IRanges_class.c */
	REGISTER_CCALLABLE(_get_IRanges_start);
	REGISTER_CCALLABLE(_get_IRanges_width);
	REGISTER_CCALLABLE(_get_IRanges_names);
	REGISTER_CCALLABLE(_get_IRanges_length);
	REGISTER_CCALLABLE(_hold_IRanges);
	REGISTER_CCALLABLE(_get_length_from_IRanges_holder);
	REGISTER_CCALLABLE(_get_width_elt_from_IRanges_holder);
	REGISTER_CCALLABLE(_get_start_elt_from_IRanges_holder);
	REGISTER_CCALLABLE(_get_end_elt_from_IRanges_holder);
	REGISTER_CCALLABLE(_get_names_elt_from_IRanges_holder);
	REGISTER_CCALLABLE(_get_linear_subset_from_IRanges_holder);
	REGISTER_CCALLABLE(_set_IRanges_names);
	REGISTER_CCALLABLE(_copy_IRanges_slots);
	REGISTER_CCALLABLE(_new_IRanges);
	REGISTER_CCALLABLE(_new_IRanges_from_RangeAE);
	REGISTER_CCALLABLE(_new_list_of_IRanges_from_RangeAEAE);
	REGISTER_CCALLABLE(_alloc_IRanges);

/* Grouping_class.c */
	REGISTER_CCALLABLE(_get_H2LGrouping_high2low);
	REGISTER_CCALLABLE(_get_H2LGrouping_low2high);
	REGISTER_CCALLABLE(_get_Partitioning_names);
	REGISTER_CCALLABLE(_get_PartitioningByEnd_end);
	REGISTER_CCALLABLE(_new_PartitioningByEnd);

/* SimpleList_class.c */
	REGISTER_CCALLABLE(_new_SimpleList);

/* DataFrame_class.c */
	REGISTER_CCALLABLE(_new_DataFrame);

/* CompressedList_class.c */
	REGISTER_CCALLABLE(_get_CompressedList_unlistData);
	REGISTER_CCALLABLE(_get_CompressedList_partitioning);
	REGISTER_CCALLABLE(_get_CompressedList_length);
	REGISTER_CCALLABLE(_get_CompressedList_names);
	REGISTER_CCALLABLE(_new_CompressedList);

/* CompressedIRangesList_class.c */
	REGISTER_CCALLABLE(_hold_CompressedIRangesList);
	REGISTER_CCALLABLE(_get_elt_from_CompressedIRangesList_holder);

/* RangedData_class.c */
	REGISTER_CCALLABLE(_new_RangedData);

/* Rle_class.c */
	REGISTER_CCALLABLE(_seqselect_Rle);
	return;
}

