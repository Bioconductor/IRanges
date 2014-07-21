#include "../inst/include/IRanges_defines.h"
#include <string.h>

#define DEBUG_IRANGES 1

#define INIT_STATIC_SYMBOL(NAME) \
{ \
	if (NAME ## _symbol == NULL) \
		NAME ## _symbol = install(# NAME); \
}


/* Vector_class.c */

const char *_get_List_elementType(SEXP x);

void _set_List_elementType(
	SEXP x,
	const char *type
);


/* Ranges_class.c */

SEXP valid_Ranges(
	SEXP x_start,
	SEXP x_end,
	SEXP x_width
);


/* Ranges_comparison.c */

int _overlap_code(
	int x_start,
	int x_width,
	int y_start,
	int y_width
);

int _invert_overlap_code(
	int code
);

SEXP Ranges_compare(
	SEXP x_start,
	SEXP x_width,
	SEXP y_start,
	SEXP y_width
);


/* IRanges_class.c */

SEXP debug_IRanges_class();

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

SEXP _new_IRanges_from_RangeAE(
	const char *classname,
	const RangeAE *range_ae
);

SEXP _new_list_of_IRanges_from_RangeAEAE(
	const char *element_type,
	const RangeAEAE *range_aeae
);

SEXP _alloc_IRanges(
	const char *classname,
	int length
);

int _is_normal_IRanges_holder(const IRanges_holder *x_holder);

SEXP IRanges_isNormal(SEXP x);

SEXP IRanges_from_integer(SEXP x);

SEXP NormalIRanges_from_logical(SEXP x);


/* IRanges_constructor.c */

SEXP solve_user_SEW0(
	SEXP start,
	SEXP end,
	SEXP width
);

SEXP solve_user_SEW(
	SEXP refwidths,
	SEXP start,
	SEXP end,
	SEXP width,
	SEXP translate_negative_coord,
	SEXP allow_nonnarrowing
);


/* Grouping_class.c */

SEXP debug_Grouping_class();

SEXP _get_H2LGrouping_high2low(SEXP x);

SEXP _get_H2LGrouping_low2high(SEXP x);

SEXP _get_Partitioning_names(SEXP x);

SEXP _get_PartitioningByEnd_end(SEXP x);

SEXP _new_PartitioningByEnd(
	const char *classname,
	SEXP end,
	SEXP names
);

SEXP H2LGrouping_members(
	SEXP x,
	SEXP group_ids
);

SEXP H2LGrouping_vmembers(
	SEXP x,
	SEXP group_ids_list
);


/* SimpleList_class.c */

SEXP _new_SimpleList(
	const char *classname,
	SEXP listData
);


/* DataFrame_class.c */

SEXP _new_DataFrame(
	const char *classname,
	SEXP vars,
	SEXP rownames,
	SEXP nrows
);


/* SimpleIRangesList_class.c */

SEXP SimpleIRangesList_isNormal(SEXP x);

SEXP SimpleNormalIRangesList_min(SEXP x);

SEXP SimpleNormalIRangesList_max(SEXP x);


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


/* RleViews_utils.c */

SEXP RleViews_viewMins(
	SEXP x,
	SEXP na_rm
);

SEXP RleViews_viewMaxs(
	SEXP x,
	SEXP na_rm
);

SEXP RleViews_viewSums(
	SEXP x,
	SEXP na_rm
);

SEXP RleViews_viewMeans(
	SEXP x,
	SEXP na_rm
);

SEXP RleViews_viewWhichMins(
	SEXP x,
	SEXP na_rm
);

SEXP RleViews_viewWhichMaxs(
	SEXP x,
	SEXP na_rm
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

int _get_eltlens_from_CompressedIRangesList_holder(
	const CompressedIRangesList_holder *x_holder,
	int i
);

SEXP CompressedIRangesList_isNormal(
	SEXP x,
	SEXP use_names
);

SEXP CompressedIRangesList_summary(
	SEXP object
);

SEXP CompressedNormalIRangesList_min(
	SEXP x,
	SEXP use_names
);

SEXP CompressedNormalIRangesList_max(
	SEXP x,
	SEXP use_names
);


/* GappedRanges_class.c */

SEXP valid_GappedRanges(SEXP x, SEXP ans_type);


/* RangedData_class.c */

SEXP _new_RangedData(
	const char *classname,
	SEXP ranges,
	SEXP values
);


/* inter_range_methods.c */

SEXP debug_inter_range_methods();

SEXP IRanges_range(SEXP x);

SEXP Ranges_reduce(
	SEXP x_start,
	SEXP x_width,
	SEXP drop_empty_ranges,
	SEXP min_gapwidth,
	SEXP with_revmap,
	SEXP with_inframe_start
);

SEXP CompressedIRangesList_reduce(
	SEXP x,
	SEXP drop_empty_ranges,
	SEXP min_gapwidth,
	SEXP with_revmap
);

SEXP IRanges_gaps(
	SEXP x_start,
	SEXP x_width,
	SEXP start,
	SEXP end
);

SEXP CompressedIRangesList_gaps(
	SEXP x,
	SEXP start,
	SEXP end
);

SEXP Ranges_disjointBins(
	SEXP x_start,
	SEXP x_width
);


/* coverage_methods.c */

SEXP IRanges_coverage(
	SEXP x,
	SEXP shift,
	SEXP width,
	SEXP weight,
	SEXP circle_len,
	SEXP method
);

SEXP CompressedIRangesList_coverage(
	SEXP x,
	SEXP shift,
	SEXP width,
	SEXP weight,
	SEXP circle_lens,
	SEXP method
);


/* NCList.c */

SEXP NCList_new();

SEXP NCList_build(
	SEXP nclist,
	SEXP x_start,
	SEXP x_end
);

SEXP NCList_print(
	SEXP x_nclist,
	SEXP x_start,
	SEXP x_end
);

SEXP NCList_free(SEXP x);

SEXP NCList_find_overlaps(
	SEXP q_start,
	SEXP q_end,
	SEXP s_nclist,
	SEXP s_start,
	SEXP s_end
);

