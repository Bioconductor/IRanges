#include "../inst/include/IRanges_defines.h"
#include <string.h>

#define DEBUG_IRANGES 1

#define INIT_STATIC_SYMBOL(NAME) \
{ \
	if (NAME ## _symbol == NULL) \
		NAME ## _symbol = install(# NAME); \
}


/* safe_arithm.c */

void _reset_ovflow_flag();

int _get_ovflow_flag();

int _safe_int_add(
	int x,
	int y
);

int _safe_int_mult(
	int x,
	int y
);


/* sort_utils.c */

void _sort_int_array(
	int *x,
	int nelt,
	int desc
);

void _get_order_of_int_array(
	const int *x,
	int nelt,
	int desc,
	int *out,
	int out_shift
);

void _get_order_of_int_pairs(
	const int *a,
	const int *b,
	int nelt,
	int desc,
	int *out,
	int out_shift
);

void _get_matches_of_ordered_int_pairs(
	const int *a1,
	const int *b1,
	const int *o1,
	int nelt1,
	const int *a2,
	const int *b2,
	const int *o2,
	int nelt2,
	int nomatch,
	int *out,
	int out_shift
);

void _get_order_of_int_quads(
	const int *a,
	const int *b,
	const int *c,
	const int *d,
	int nelt,
	int desc,
	int *out,
	int out_shift
);

void _get_matches_of_ordered_int_quads(
	const int *a1,
	const int *b1,
	const int *c1,
	const int *d1,
	const int *o1,
	int nelt1,
	const int *a2,
	const int *b2,
	const int *c2,
	const int *d2,
	const int *o2,
	int nelt2,
	int nomatch,
	int *out,
	int out_shift
);


/* hash_utils.c */

struct htab _new_htab(int n);

int _get_hbucket_val(
	const struct htab *htab,
	int bucket_idx
);

void _set_hbucket_val(
	struct htab *htab,
	int bucket_idx,
	int val
);


/* AEbufs.c */

SEXP debug_AEbufs();

SEXP AEbufs_use_malloc(SEXP x);

int _get_new_buflength(int buflength);

int _IntAE_get_nelt(const IntAE *int_ae);

int _IntAE_set_nelt(
	IntAE *int_ae,
	int nelt
);

void _IntAE_set_val(
	const IntAE *int_ae,
	int val
);

IntAE _new_IntAE(
	int buflength,
	int nelt,
	int val
);

void _IntAE_insert_at(
	IntAE *int_ae,
	int at,
	int val
);

void _IntAE_append(
	IntAE *int_ae,
	const int *newvals,
	int nnewval
);

void _IntAE_delete_at(
	IntAE *int_ae,
	int at
);

void _IntAE_shift(
	const IntAE *int_ae,
	int shift
);

void _IntAE_sum_and_shift(
	const IntAE *int_ae1,
	const IntAE *int_ae2,
	int shift
);

void _IntAE_append_shifted_vals(
	IntAE *int_ae,
	const int *newvals,
	int nnewval,
	int shift
);

void _IntAE_qsort(
	const IntAE *int_ae,
	int desc
);

void _IntAE_delete_adjdups(IntAE *int_ae);

SEXP _new_INTEGER_from_IntAE(const IntAE *int_ae);

IntAE _new_IntAE_from_INTEGER(SEXP x);

IntAE _new_IntAE_from_CHARACTER(
	SEXP x,
	int keyshift
);

int _IntAEAE_get_nelt(const IntAEAE *int_aeae);

int _IntAEAE_set_nelt(
	IntAEAE *int_aeae,
	int nelt
);

IntAEAE _new_IntAEAE(
	int buflength,
	int nelt
);

void _IntAEAE_insert_at(
	IntAEAE *int_aeae,
	int at,
	const IntAE *int_ae
);

void _IntAEAE_eltwise_append(
	const IntAEAE *int_aeae1,
	const IntAEAE *int_aeae2
);

void _IntAEAE_shift(
	const IntAEAE *int_aeae,
	int shift
);

void _IntAEAE_sum_and_shift(
	const IntAEAE *int_aeae1,
	const IntAEAE *int_aeae2,
	int shift
);

SEXP _new_LIST_from_IntAEAE(
	const IntAEAE *int_aeae,
	int mode
);

IntAEAE _new_IntAEAE_from_LIST(SEXP x);

SEXP _IntAEAE_toEnvir(
	const IntAEAE *int_aeae,
	SEXP envir,
	int keyshift
);

int _RangeAE_get_nelt(const RangeAE *range_ae);

int _RangeAE_set_nelt(
	RangeAE *range_ae,
	int nelt
);

RangeAE _new_RangeAE(
	int buflength,
	int nelt
);

void _RangeAE_insert_at(
	RangeAE *range_ae,
	int at,
	int start,
	int width
);

int _RangeAEAE_get_nelt(const RangeAEAE *range_aeae);

int _RangeAEAE_set_nelt(
	RangeAEAE *range_aeae,
	int nelt
);

RangeAEAE _new_RangeAEAE(
	int buflength,
	int nelt
);

void _RangeAEAE_insert_at(
	RangeAEAE *range_aeae,
	int at,
	const RangeAE *range_ae
);

int _CharAE_get_nelt(const CharAE *char_ae);

int _CharAE_set_nelt(
	CharAE *char_ae,
	int nelt
);

CharAE _new_CharAE(int buflength);

CharAE _new_CharAE_from_string(const char *string);

void _CharAE_insert_at(
	CharAE *char_ae,
	int at,
	char c
);

void _append_string_to_CharAE(
	CharAE *char_ae,
	const char *string
);

void _CharAE_delete_at(
	CharAE *char_ae,
	int at,
	int nelt
);

SEXP _new_RAW_from_CharAE(const CharAE *char_ae);

SEXP _new_LOGICAL_from_CharAE(const CharAE *char_ae);

int _CharAEAE_get_nelt(const CharAEAE *char_aeae);

int _CharAEAE_set_nelt(
	CharAEAE *char_aeae,
	int nelt
);

CharAEAE _new_CharAEAE(
	int buflength,
	int nelt
);

void _CharAEAE_insert_at(
	CharAEAE *char_aeae,
	int at,
	const CharAE *char_ae
);

void _append_string_to_CharAEAE(
	CharAEAE *char_aeae,
	const char *string
);

SEXP _new_CHARACTER_from_CharAEAE(const CharAEAE *char_aeae);

SEXP AEbufs_free();


/* anyMissing.c */

SEXP anyMissing(SEXP x);


/* SEXP_utils.c */

const char *_get_classname(SEXP x);

SEXP sapply_NROW(SEXP x);


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

cachedIRanges _cache_IRanges(SEXP x);

int _get_cachedIRanges_length(const cachedIRanges *cached_x);

int _get_cachedIRanges_elt_width(
	const cachedIRanges *cached_x,
	int i
);

int _get_cachedIRanges_elt_start(
	const cachedIRanges *cached_x,
	int i
);

int _get_cachedIRanges_elt_end(
	const cachedIRanges *cached_x,
	int i
);

SEXP _get_cachedIRanges_elt_name(
	const cachedIRanges *cached_x,
	int i
);

cachedIRanges _sub_cachedIRanges(
	const cachedIRanges *cached_x,
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

int _is_normal_cachedIRanges(const cachedIRanges *cached_ir);

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

cachedCompressedIRangesList _cache_CompressedIRangesList(SEXP x);

int _get_cachedCompressedIRangesList_length(
	const cachedCompressedIRangesList *cached_x
);

cachedIRanges _get_cachedCompressedIRangesList_elt(
	const cachedCompressedIRangesList *cached_x,
	int i
);

int _get_cachedCompressedIRangesList_eltLength(
	const cachedCompressedIRangesList *cached_x,
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
	SEXP with_mapping,
	SEXP with_inframe_start
);

SEXP CompressedIRangesList_reduce(
	SEXP x,
	SEXP drop_empty_ranges,
	SEXP min_gapwidth,
	SEXP with_mapping
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


/* encodeOverlaps_methods.c */

SEXP encode_overlaps1(
	SEXP query_start,
	SEXP query_width,
	SEXP query_space,
	SEXP query_break,
	SEXP flip_query,
	SEXP subject_start,
	SEXP subject_width,
	SEXP subject_space,
	SEXP as_matrix,
	SEXP as_raw
);

SEXP RangesList_encode_overlaps(
	SEXP query_starts,
	SEXP query_widths,
	SEXP query_spaces,
	SEXP query_breaks,
	SEXP subject_starts,
	SEXP subject_widths,
	SEXP subject_spaces
);

SEXP Hits_encode_overlaps(
	SEXP query_starts,
	SEXP query_widths,
	SEXP query_spaces,
	SEXP query_breaks,
	SEXP subject_starts,
	SEXP subject_widths,
	SEXP subject_spaces,
	SEXP query_hits,
	SEXP subject_hits,
	SEXP flip_query
);


/* coverage_methods.c */

SEXP IRanges_coverage(
	SEXP x,
	SEXP shift,
	SEXP width,
	SEXP weight,
	SEXP method
);

SEXP CompressedIRangesList_coverage(
	SEXP x,
	SEXP shift,
	SEXP width,
	SEXP weight,
	SEXP method
);

