#include "IRanges.h"

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#define REGISTER_CCALLABLE(fun) \
	R_RegisterCCallable("IRanges", #fun, (DL_FUNC) &fun)

static const R_CallMethodDef callMethods[] = {

/* Ranges_class.c */
	CALLMETHOD_DEF(valid_Ranges, 3),

/* Ranges_comparison.c */
	CALLMETHOD_DEF(Ranges_pcompare, 4),

/* IRanges_class.c */
	CALLMETHOD_DEF(IRanges_isNormal, 1),
	CALLMETHOD_DEF(IRanges_from_integer, 1),
	CALLMETHOD_DEF(NormalIRanges_from_logical, 1),

/* IRanges_constructor.c */
	CALLMETHOD_DEF(solve_user_SEW0, 3),
	CALLMETHOD_DEF(solve_user_SEW, 6),

/* Grouping_class.c */
	CALLMETHOD_DEF(H2LGrouping_members, 2),
	CALLMETHOD_DEF(H2LGrouping_vmembers, 2),

/* RleViews_utils.c */
	CALLMETHOD_DEF(RleViews_viewMins, 2),
	CALLMETHOD_DEF(RleViews_viewMaxs, 2),
	CALLMETHOD_DEF(RleViews_viewSums, 2),
	CALLMETHOD_DEF(RleViews_viewMeans, 2),
	CALLMETHOD_DEF(RleViews_viewWhichMins, 2),
	CALLMETHOD_DEF(RleViews_viewWhichMaxs, 2),

/* SimpleIRangesList_class.c */
	CALLMETHOD_DEF(SimpleIRangesList_isNormal, 2),
	CALLMETHOD_DEF(SimpleNormalIRangesList_min, 1),
	CALLMETHOD_DEF(SimpleNormalIRangesList_max, 1),

/* CompressedIRangesList_class.c */
	CALLMETHOD_DEF(CompressedIRangesList_isNormal, 2),
	CALLMETHOD_DEF(CompressedIRangesList_summary, 1),
	CALLMETHOD_DEF(CompressedNormalIRangesList_min, 2),
	CALLMETHOD_DEF(CompressedNormalIRangesList_max, 2),

/* inter_range_methods.c */
	CALLMETHOD_DEF(IRanges_range, 1),
	CALLMETHOD_DEF(Ranges_reduce, 6),
	CALLMETHOD_DEF(CompressedIRangesList_reduce, 4),
	CALLMETHOD_DEF(IRanges_gaps, 4),
	CALLMETHOD_DEF(CompressedIRangesList_gaps, 3),
	CALLMETHOD_DEF(Ranges_disjointBins, 2),

/* coverage_methods.c */
	CALLMETHOD_DEF(IRanges_coverage, 6),
	CALLMETHOD_DEF(CompressedIRangesList_coverage, 6),

/* NCList.c */
	CALLMETHOD_DEF(NCList_new, 0),
	CALLMETHOD_DEF(NCList_free, 1),
	CALLMETHOD_DEF(NCList_build, 4),
	CALLMETHOD_DEF(new_NCListAsINTSXP_from_NCList, 1),
	CALLMETHOD_DEF(NCListAsINTSXP_print, 3),
	CALLMETHOD_DEF(NCList_find_overlaps, 11),
	CALLMETHOD_DEF(NCList_find_overlaps_in_groups, 15),

/* CompressedAtomicList_utils.c */
	CALLMETHOD_DEF(CompressedLogicalList_sum, 2),
	CALLMETHOD_DEF(CompressedIntegerList_sum, 2),
	CALLMETHOD_DEF(CompressedNumericList_sum, 2),
	CALLMETHOD_DEF(CompressedLogicalList_prod, 2),
	CALLMETHOD_DEF(CompressedIntegerList_prod, 2),
	CALLMETHOD_DEF(CompressedNumericList_prod, 2),
	CALLMETHOD_DEF(CompressedLogicalList_min, 2),
	CALLMETHOD_DEF(CompressedLogicalList_which_min, 1),
	CALLMETHOD_DEF(CompressedIntegerList_min, 2),
	CALLMETHOD_DEF(CompressedIntegerList_which_min, 1),
	CALLMETHOD_DEF(CompressedNumericList_min, 2),
	CALLMETHOD_DEF(CompressedNumericList_which_min, 1),
	CALLMETHOD_DEF(CompressedLogicalList_max, 2),
	CALLMETHOD_DEF(CompressedLogicalList_which_max, 1),
	CALLMETHOD_DEF(CompressedIntegerList_max, 2),
	CALLMETHOD_DEF(CompressedIntegerList_which_max, 1),
	CALLMETHOD_DEF(CompressedNumericList_max, 2),
	CALLMETHOD_DEF(CompressedNumericList_which_max, 1),
	CALLMETHOD_DEF(CompressedLogicalList_is_unsorted, 3),
	CALLMETHOD_DEF(CompressedIntegerList_is_unsorted, 3),
	CALLMETHOD_DEF(CompressedNumericList_is_unsorted, 3),

	{NULL, NULL, 0}
};


void R_init_IRanges(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);

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

