#include "../inst/include/IRanges_defines.h"
#include <string.h>

#define DEBUG_IRANGES 1

#define INIT_STATIC_SYMBOL(NAME) \
{ \
	if (NAME ## _symbol == NULL) \
		NAME ## _symbol = install(# NAME); \
}


/* SEXP_utils.c */

const char *_get_classname(SEXP x);

SEXP sapply_NROW(SEXP x);

SEXP top_prenv(SEXP nm, SEXP env);

SEXP top_prenv_dots(SEXP env);

/* int_utils.c */

SEXP Integer_any_missing_or_outside(SEXP x, SEXP lower, SEXP upper);

int _sum_non_neg_ints(
	const int *x,
	int x_len,
	const char *varname
);

SEXP Integer_sum_non_neg_vals(SEXP x);

SEXP Integer_diff_with_0(SEXP x);

SEXP Integer_diff_with_last(SEXP x, SEXP last);

SEXP Integer_order(
	SEXP x,
	SEXP decreasing
);

int _check_integer_pairs(
	SEXP a,
	SEXP b,
	const int **a_p,
	const int **b_p,
	const char *a_argname,
	const char *b_argname
);

SEXP Integer_order2(
	SEXP a,
	SEXP b,
	SEXP decreasing
);

SEXP Integer_match2_quick(
	SEXP a1,
	SEXP b1,
	SEXP a2,
	SEXP b2,
	SEXP nomatch
);

SEXP Integer_selfmatch2_quick(
	SEXP a,
	SEXP b
);

SEXP Integer_match2_hash(
	SEXP a1,
	SEXP b1,
	SEXP a2,
	SEXP b2,
	SEXP nomatch
);

SEXP Integer_selfmatch2_hash(
	SEXP a,
	SEXP b
);

int _check_integer_quads(
	SEXP a,
	SEXP b,
	SEXP c,
	SEXP d,
	const int **a_p,
	const int **b_p,
	const int **c_p,
	const int **d_p,
	const char *a_argname,
	const char *b_argname,
	const char *c_argname,
	const char *d_argname
);

SEXP Integer_order4(
	SEXP a,
	SEXP b,
	SEXP c,
	SEXP d,
	SEXP decreasing
);

SEXP Integer_match4_quick(
	SEXP a1,
	SEXP b1,
	SEXP c1,
	SEXP d1,
	SEXP a2,
	SEXP b2,
	SEXP c2,
	SEXP d2,
	SEXP nomatch
);

SEXP Integer_selfmatch4_quick(
	SEXP a,
	SEXP b,
	SEXP c,
	SEXP d
);

SEXP Integer_match4_hash(
	SEXP a1,
	SEXP b1,
	SEXP c1,
	SEXP d1,
	SEXP a2,
	SEXP b2,
	SEXP c2,
	SEXP d2,
	SEXP nomatch
);

SEXP Integer_selfmatch4_hash(
	SEXP a,
	SEXP b,
	SEXP c,
	SEXP d
);

SEXP Integer_tabulate2(
	SEXP x,
	SEXP nbins,
	SEXP weight,
	SEXP strict
);

SEXP Integer_explode_bits(
	SEXP x,
	SEXP bitpos
);

SEXP Integer_sorted_merge(
	SEXP x,
	SEXP y
);

SEXP Integer_mseq(
	SEXP from,
	SEXP to
);

SEXP Integer_fancy_mseq(
	SEXP lengths,
	SEXP offset,
	SEXP rev
);

SEXP _find_interv_and_start_from_width(
	const int *x,
	int x_len,
	const int *width,
	int width_len
);

SEXP findIntervalAndStartFromWidth(
	SEXP x,
	SEXP vec
);


/* str_utils.c */

SEXP unstrsplit_list(SEXP x, SEXP sep);

SEXP safe_strexplode(SEXP s);

SEXP strsplit_as_list_of_ints(SEXP x, SEXP sep);

SEXP svn_time();


/* compact_bitvector.c */

SEXP logical_as_compact_bitvector(SEXP x);

SEXP compact_bitvector_as_logical(SEXP x, SEXP length_out);

SEXP subset_compact_bitvector(SEXP x, SEXP subscript);

SEXP compact_bitvector_bit_count(SEXP x);

SEXP compact_bitvector_last_bit(SEXP x);

SEXP compact_bitvector_set_op(SEXP query, SEXP ref, SEXP align);


/* Vector_class.c */

const char *_get_List_elementType(SEXP x);

void _set_List_elementType(
	SEXP x,
	const char *type
);

int _vector_memcmp(
	SEXP x1,
	int x1_offset,
	SEXP x2,
	int x2_offset,
	int nelt
);

void _vector_memcpy(
	SEXP out,
	int out_offset,
	SEXP in,
	int in_offset,
	int nelt
);

SEXP vector_subsetByRanges(
	SEXP x,
	SEXP start,
	SEXP width
);

SEXP vector_seqselect(
	SEXP x,
	SEXP start,
	SEXP width
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


/* Rle_class.c */

SEXP _logical_Rle_constructor(
	const int *values,
	int nvalues,
	const int *lengths,
	int buflength
);

SEXP _integer_Rle_constructor(
	const int *values,
	int nvalues,
	const int *lengths,
	int buflength
);

SEXP _numeric_Rle_constructor(
	const double *values,
	int nvalues,
	const int *lengths,
	int buflength
);

SEXP _complex_Rle_constructor(
	const Rcomplex *values,
	int nvalues,
	const int *lengths,
	int buflength
);

SEXP _character_Rle_constructor(
	SEXP values,
	const int *lengths,
	int buflength
);

SEXP _raw_Rle_constructor(
	const Rbyte *values,
	int nvalues,
	const int *lengths,
	int buflength
);

SEXP Rle_constructor(
	SEXP values,
	SEXP lengths,
	SEXP check,
	SEXP buflength
);

SEXP Rle_start(SEXP x);

SEXP Rle_end(SEXP x);

SEXP Rle_getStartEndRunAndOffset(
	SEXP x,
	SEXP start,
	SEXP end
);

SEXP Rle_window_aslist(
	SEXP x,
	SEXP runStart,
	SEXP runEnd,
	SEXP offsetStart,
	SEXP offsetEnd
);

SEXP Rle_window(
	SEXP x,
	SEXP runStart,
	SEXP runEnd,
	SEXP offsetStart,
	SEXP offsetEnd,
	SEXP ans
);

SEXP _seqselect_Rle(
	SEXP x,
	const int *start,
	const int *width,
	int length
);

SEXP Rle_seqselect(
	SEXP x,
	SEXP start,
	SEXP width
);


/* Rle_utils.c */

SEXP Rle_runsum(
	SEXP x,
	SEXP k,
	SEXP na_rm 
);

SEXP Rle_runwtsum(
	SEXP x,
	SEXP k,
	SEXP wt,
	SEXP na_rm 
);

SEXP Rle_runq(
	SEXP x,
	SEXP k,
	SEXP which,
	SEXP na_rm 
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


/* Hits_class.c */

SEXP make_all_group_inner_hits(
	SEXP group_sizes,
	SEXP hit_type
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

