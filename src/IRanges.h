#include "../inst/include/IRanges_defines.h"
#include <string.h>

#define INIT_STATIC_SYMBOL(NAME) \
{ \
	if (NAME ## _symbol == NULL) \
		NAME ## _symbol = install(# NAME); \
}


/* Ranges_class.c */

SEXP C_validate_Ranges(
	SEXP x_start,
	SEXP x_end,
	SEXP x_width
);


/* IPosRanges_comparison.c */

int _overlap_code(
	int x_start,
	int x_width,
	int y_start,
	int y_width
);

int _invert_overlap_code(
	int code
);

SEXP C_pcompare_IPosRanges(
	SEXP x_start,
	SEXP x_width,
	SEXP y_start,
	SEXP y_width
);


/* IRanges_class.c */

SEXP _get_IRanges_start(SEXP x);

SEXP _get_IRanges_width(SEXP x);

SEXP _get_IRanges_names(SEXP x);

int _get_IRanges_length(SEXP x);

IRanges_holder _hold_IRanges(SEXP x);

int _get_length_from_IRanges_holder(const IRanges_holder *x_holder);

int _get_width_elt_from_IRanges_holder(
	const IRanges_holder *x_holder,
	int i
);

int _get_start_elt_from_IRanges_holder(
	const IRanges_holder *x_holder,
	int i
);

int _get_end_elt_from_IRanges_holder(
	const IRanges_holder *x_holder,
	int i
);

SEXP _get_names_elt_from_IRanges_holder(
	const IRanges_holder *x_holder,
	int i
);

IRanges_holder _get_linear_subset_from_IRanges_holder(
	const IRanges_holder *x_holder,
	int offset,
	int length
);

void _set_IRanges_names(
	SEXP x,
	SEXP names
);

void _copy_IRanges_slots(
	SEXP x,
	SEXP x0
);

SEXP _new_IRanges(
	const char *classname,
	SEXP start,
	SEXP width,
	SEXP names
);

SEXP _new_IRanges_from_IntPairAE(
	const char *classname,
	const IntPairAE *intpair_ae
);

SEXP _new_list_of_IRanges_from_IntPairAEAE(
	const char *element_type,
	const IntPairAEAE *intpair_aeae
);

SEXP _alloc_IRanges(
	const char *classname,
	int length
);

int _is_normal_IRanges_holder(const IRanges_holder *x_holder);

SEXP C_isNormal_IRanges(SEXP x);

SEXP C_from_integer_to_IRanges(SEXP x);

SEXP C_from_logical_to_NormalIRanges(SEXP x);


/* IRanges_constructor.c */

SEXP C_solve_start_end_width(
	SEXP start,
	SEXP end,
	SEXP width
);

SEXP C_solve_user_SEW(
	SEXP refwidths,
	SEXP start,
	SEXP end,
	SEXP width,
	SEXP translate_negative_coord,
	SEXP allow_nonnarrowing
);


/* Grouping_class.c */

SEXP _get_H2LGrouping_high2low(SEXP x);

SEXP _get_H2LGrouping_low2high(SEXP x);

SEXP _get_Partitioning_names(SEXP x);

SEXP _get_PartitioningByEnd_end(SEXP x);

SEXP _new_PartitioningByEnd(
	const char *classname,
	SEXP end,
	SEXP names
);

SEXP C_members_H2LGrouping(
	SEXP x,
	SEXP group_ids
);

SEXP C_vmembers_H2LGrouping(
	SEXP x,
	SEXP group_ids_list
);


/* RleViews_utils.c */

SEXP C_viewMins_RleViews(
	SEXP x,
	SEXP na_rm
);

SEXP C_viewMaxs_RleViews(
	SEXP x,
	SEXP na_rm
);

SEXP C_viewSums_RleViews(
	SEXP x,
	SEXP na_rm
);

SEXP C_viewMeans_RleViews(
	SEXP x,
	SEXP na_rm
);

SEXP C_viewWhichMins_RleViews(
	SEXP x,
	SEXP na_rm
);

SEXP C_viewWhichMaxs_RleViews(
	SEXP x,
	SEXP na_rm
);


/* SimpleIRangesList_class.c */

SEXP C_isNormal_SimpleIRangesList(SEXP x, SEXP use_names);

SEXP C_min_SimpleNormalIRangesList(SEXP x);

SEXP C_max_SimpleNormalIRangesList(SEXP x);


/* CompressedList_class.c */

SEXP _get_CompressedList_unlistData(SEXP x);

SEXP _get_CompressedList_partitioning(SEXP x);

int _get_CompressedList_length(SEXP x);

SEXP _get_CompressedList_names(SEXP x);

SEXP _new_CompressedList(
	const char *classname,
	SEXP unlistData,
	SEXP partitioning
);

CompressedIntsList_holder _hold_CompressedIntegerList(
	SEXP x
);

int _get_length_from_CompressedIntsList_holder(
	const CompressedIntsList_holder *x_holder
);

Ints_holder _get_elt_from_CompressedIntsList_holder(
	const CompressedIntsList_holder *x_holder,
	int i
);


/* CompressedIRangesList_class.c */

CompressedIRangesList_holder _hold_CompressedIRangesList(SEXP x);

int _get_length_from_CompressedIRangesList_holder(
	const CompressedIRangesList_holder *x_holder
);

IRanges_holder _get_elt_from_CompressedIRangesList_holder(
	const CompressedIRangesList_holder *x_holder,
	int i
);

int _get_eltNROWS_from_CompressedIRangesList_holder(
	const CompressedIRangesList_holder *x_holder,
	int i
);

SEXP C_isNormal_CompressedIRangesList(
	SEXP x,
	SEXP use_names
);

SEXP C_summary_CompressedIRangesList(
	SEXP object
);

SEXP C_min_CompressedNormalIRangesList(
	SEXP x,
	SEXP use_names
);

SEXP C_max_CompressedNormalIRangesList(
	SEXP x,
	SEXP use_names
);


/* inter_range_methods.c */

SEXP C_range_IRanges(SEXP x);

SEXP C_reduce_IntegerRanges(
	SEXP x_start,
	SEXP x_width,
	SEXP drop_empty_ranges,
	SEXP min_gapwidth,
	SEXP with_revmap,
	SEXP with_inframe_start
);

SEXP C_reduce_CompressedIRangesList(
	SEXP x,
	SEXP drop_empty_ranges,
	SEXP min_gapwidth,
	SEXP with_revmap
);

SEXP C_gaps_IntegerRanges(
	SEXP x_start,
	SEXP x_width,
	SEXP start,
	SEXP end
);

SEXP C_gaps_CompressedIRangesList(
	SEXP x,
	SEXP start,
	SEXP end
);

SEXP C_disjointBins_IntegerRanges(
	SEXP x_start,
	SEXP x_width
);


/* coverage_methods.c */

SEXP C_coverage_IRanges(
	SEXP x,
	SEXP shift,
	SEXP width,
	SEXP weight,
	SEXP circle_len,
	SEXP method
);

SEXP C_coverage_CompressedIRangesList(
	SEXP x,
	SEXP shift,
	SEXP width,
	SEXP weight,
	SEXP circle_lens,
	SEXP method
);


/* NCList.c */

SEXP C_new_NCList();

SEXP C_free_NCList(SEXP nclist_xp);

SEXP C_build_NCList(
	SEXP nclist_xp,
	SEXP x_start,
	SEXP x_end,
	SEXP x_subset
);

SEXP C_new_NCListAsINTSXP_from_NCList(SEXP nclist_xp);

SEXP C_print_NCListAsINTSXP(
	SEXP x_nclist,
	SEXP x_start,
	SEXP x_end
);

SEXP C_find_overlaps_NCList(
	SEXP q_start,
	SEXP q_end,
	SEXP s_start,
	SEXP s_end,
	SEXP nclist,
	SEXP nclist_is_q,
	SEXP maxgap,
	SEXP minoverlap,
	SEXP type,
	SEXP select,
	SEXP circle_length
);

SEXP C_find_overlaps_in_groups_NCList(
	SEXP q_start,
	SEXP q_end,
	SEXP q_space,
	SEXP q_groups,
	SEXP s_start,
	SEXP s_end,
	SEXP s_space,
	SEXP s_groups,
	SEXP nclists,
	SEXP nclist_is_q,
	SEXP maxgap,
	SEXP minoverlap,
	SEXP type,
	SEXP select,
	SEXP circle_length
);

/* CompressedAtomicList_utils.c */

SEXP C_sum_CompressedLogicalList(
	SEXP x,
	SEXP na_rm
);

SEXP C_sum_CompressedIntegerList(
	SEXP x,
	SEXP na_rm
);

SEXP C_sum_CompressedNumericList(
	SEXP x,
	SEXP na_rm
);

SEXP C_prod_CompressedLogicalList(
	SEXP x,
	SEXP na_rm
);

SEXP C_prod_CompressedIntegerList(
	SEXP x,
	SEXP na_rm
);

SEXP C_prod_CompressedNumericList(
	SEXP x,
	SEXP na_rm
);

SEXP C_min_CompressedLogicalList(
	SEXP x,
	SEXP na_rm
);

SEXP C_min_CompressedIntegerList(
	SEXP x,
	SEXP na_rm
);

SEXP C_min_CompressedNumericList(
	SEXP x,
	SEXP na_rm
);

SEXP C_max_CompressedLogicalList(
	SEXP x,
	SEXP na_rm
);

SEXP C_max_CompressedIntegerList(
	SEXP x,
	SEXP na_rm
);

SEXP C_max_CompressedNumericList(
	SEXP x,
	SEXP na_rm
);

SEXP C_which_min_CompressedLogicalList(SEXP x);

SEXP C_which_min_CompressedIntegerList(SEXP x);

SEXP C_which_min_CompressedNumericList(SEXP x);

SEXP C_which_max_CompressedLogicalList(SEXP x);

SEXP C_which_max_CompressedIntegerList(SEXP x);

SEXP C_which_max_CompressedNumericList(SEXP x);

SEXP C_is_unsorted_CompressedLogicalList(
	SEXP x,
	SEXP na_rm,
	SEXP strictly
);

SEXP C_is_unsorted_CompressedIntegerList(
	SEXP x,
	SEXP na_rm,
	SEXP strictly
);

SEXP C_is_unsorted_CompressedNumericList(
	SEXP x,
	SEXP na_rm,
	SEXP strictly
);

/* extractListFragments.c */

SEXP C_find_partition_overlaps(
	SEXP q_end,
	SEXP s_end,
	SEXP with_split_partitions
);

