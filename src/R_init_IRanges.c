#include "IRanges.h"

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#define REGISTER_CCALLABLE(fun) \
	R_RegisterCCallable("IRanges", #fun, (DL_FUNC) &fun)

static const R_CallMethodDef callMethods[] = {

/* Ranges_class.c */
	CALLMETHOD_DEF(C_validate_Ranges, 3),

/* IPosRanges_comparison.c */
	CALLMETHOD_DEF(C_pcompare_IPosRanges, 4),

/* IRanges_class.c */
	CALLMETHOD_DEF(C_isNormal_IRanges, 1),
	CALLMETHOD_DEF(C_from_integer_to_IRanges, 1),
	CALLMETHOD_DEF(C_from_logical_to_NormalIRanges, 1),

/* IRanges_constructor.c */
	CALLMETHOD_DEF(C_solve_user_SEW0, 3),
	CALLMETHOD_DEF(C_solve_user_SEW, 6),

/* Grouping_class.c */
	CALLMETHOD_DEF(C_members_H2LGrouping, 2),
	CALLMETHOD_DEF(C_vmembers_H2LGrouping, 2),

/* RleViews_utils.c */
	CALLMETHOD_DEF(C_viewMins_RleViews, 2),
	CALLMETHOD_DEF(C_viewMaxs_RleViews, 2),
	CALLMETHOD_DEF(C_viewSums_RleViews, 2),
	CALLMETHOD_DEF(C_viewMeans_RleViews, 2),
	CALLMETHOD_DEF(C_viewWhichMins_RleViews, 2),
	CALLMETHOD_DEF(C_viewWhichMaxs_RleViews, 2),

/* SimpleIRangesList_class.c */
	CALLMETHOD_DEF(C_isNormal_SimpleIRangesList, 2),
	CALLMETHOD_DEF(C_min_SimpleNormalIRangesList, 1),
	CALLMETHOD_DEF(C_max_SimpleNormalIRangesList, 1),

/* CompressedIRangesList_class.c */
	CALLMETHOD_DEF(C_isNormal_CompressedIRangesList, 2),
	CALLMETHOD_DEF(C_summary_CompressedIRangesList, 1),
	CALLMETHOD_DEF(C_min_CompressedNormalIRangesList, 2),
	CALLMETHOD_DEF(C_max_CompressedNormalIRangesList, 2),

/* inter_range_methods.c */
	CALLMETHOD_DEF(C_range_IRanges, 1),
	CALLMETHOD_DEF(C_reduce_IntegerRanges, 6),
	CALLMETHOD_DEF(C_reduce_CompressedIRangesList, 4),
	CALLMETHOD_DEF(C_gaps_IntegerRanges, 4),
	CALLMETHOD_DEF(C_gaps_CompressedIRangesList, 3),
	CALLMETHOD_DEF(C_disjointBins_IntegerRanges, 2),

/* coverage_methods.c */
	CALLMETHOD_DEF(C_coverage_IRanges, 6),
	CALLMETHOD_DEF(C_coverage_CompressedIRangesList, 6),

/* NCList.c */
	CALLMETHOD_DEF(C_new_NCList, 0),
	CALLMETHOD_DEF(C_free_NCList, 1),
	CALLMETHOD_DEF(C_build_NCList, 4),
	CALLMETHOD_DEF(C_new_NCListAsINTSXP_from_NCList, 1),
	CALLMETHOD_DEF(C_print_NCListAsINTSXP, 3),
	CALLMETHOD_DEF(C_find_overlaps_NCList, 11),
	CALLMETHOD_DEF(C_find_overlaps_in_groups_NCList, 15),

/* CompressedAtomicList_utils.c */
	CALLMETHOD_DEF(C_sum_CompressedLogicalList, 2),
	CALLMETHOD_DEF(C_sum_CompressedIntegerList, 2),
	CALLMETHOD_DEF(C_sum_CompressedNumericList, 2),
	CALLMETHOD_DEF(C_prod_CompressedLogicalList, 2),
	CALLMETHOD_DEF(C_prod_CompressedIntegerList, 2),
	CALLMETHOD_DEF(C_prod_CompressedNumericList, 2),
	CALLMETHOD_DEF(C_min_CompressedLogicalList, 2),
	CALLMETHOD_DEF(C_min_CompressedIntegerList, 2),
	CALLMETHOD_DEF(C_min_CompressedNumericList, 2),
	CALLMETHOD_DEF(C_max_CompressedLogicalList, 2),
	CALLMETHOD_DEF(C_max_CompressedIntegerList, 2),
	CALLMETHOD_DEF(C_max_CompressedNumericList, 2),
	CALLMETHOD_DEF(C_which_min_CompressedLogicalList, 1),
	CALLMETHOD_DEF(C_which_min_CompressedIntegerList, 1),
	CALLMETHOD_DEF(C_which_min_CompressedNumericList, 1),
	CALLMETHOD_DEF(C_which_max_CompressedLogicalList, 1),
	CALLMETHOD_DEF(C_which_max_CompressedIntegerList, 1),
	CALLMETHOD_DEF(C_which_max_CompressedNumericList, 1),
	CALLMETHOD_DEF(C_is_unsorted_CompressedLogicalList, 3),
	CALLMETHOD_DEF(C_is_unsorted_CompressedIntegerList, 3),
	CALLMETHOD_DEF(C_is_unsorted_CompressedNumericList, 3),

/* extractListFragments.c */
	CALLMETHOD_DEF(C_find_partition_overlaps, 3),

	{NULL, NULL, 0}
};


void R_init_IRanges(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);

/* IPosRanges_comparison.c */
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
	REGISTER_CCALLABLE(_new_IRanges_from_IntPairAE);
	REGISTER_CCALLABLE(_new_list_of_IRanges_from_IntPairAEAE);
	REGISTER_CCALLABLE(_alloc_IRanges);

/* Grouping_class.c */
	REGISTER_CCALLABLE(_get_H2LGrouping_high2low);
	REGISTER_CCALLABLE(_get_H2LGrouping_low2high);
	REGISTER_CCALLABLE(_get_Partitioning_names);
	REGISTER_CCALLABLE(_get_PartitioningByEnd_end);
	REGISTER_CCALLABLE(_new_PartitioningByEnd);

/* CompressedList_class.c */
	REGISTER_CCALLABLE(_get_CompressedList_unlistData);
	REGISTER_CCALLABLE(_get_CompressedList_partitioning);
	REGISTER_CCALLABLE(_get_CompressedList_length);
	REGISTER_CCALLABLE(_get_CompressedList_names);
	REGISTER_CCALLABLE(_new_CompressedList);
	REGISTER_CCALLABLE(_hold_CompressedIntegerList);
	REGISTER_CCALLABLE(_get_length_from_CompressedIntsList_holder);
	REGISTER_CCALLABLE(_get_elt_from_CompressedIntsList_holder);

/* CompressedIRangesList_class.c */
	REGISTER_CCALLABLE(_hold_CompressedIRangesList);
	REGISTER_CCALLABLE(_get_length_from_CompressedIRangesList_holder);
	REGISTER_CCALLABLE(_get_elt_from_CompressedIRangesList_holder);
	REGISTER_CCALLABLE(_get_eltNROWS_from_CompressedIRangesList_holder);

	return;
}

