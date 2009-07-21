#include "../inst/include/IRanges_defines.h"
#include <string.h>

#define DEBUG_IRANGES 1


/* sort_utils.c */

void _sort_int_array(
	int *x,
	int x_nelt
);

void _get_int_array_order(
	const int *x,
	int x_nelt,
	int *order
);


/* AEbufs.c */

SEXP debug_AEbufs();

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

void _IntAE_qsort(IntAE *int_ae);

void _IntAE_delete_adjdups(IntAE *int_ae);

SEXP _IntAE_asINTEGER(const IntAE *int_ae);

IntAE _INTEGER_asIntAE(SEXP x);

IntAE _CHARACTER_asIntAE(
	SEXP x,
	int keyshift
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

SEXP _IntAEAE_asLIST(
	const IntAEAE *int_aeae,
	int mode
);

IntAEAE _LIST_asIntAEAE(SEXP x);

SEXP _IntAEAE_toEnvir(
	const IntAEAE *int_aeae,
	SEXP envir,
	int keyshift
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

SEXP _RangeAE_asIRanges(const RangeAE *range_ae);

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

SEXP _CharAE_asRAW(const CharAE *char_ae);

SEXP _CharAE_asLOGICAL(const CharAE *char_ae);

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

SEXP _CharAEAE_asCHARACTER(const CharAEAE *char_aeae);

/* memcpy_utils.c */

SEXP debug_memcpy_utils();

int _IRanges_memcmp(
	const char *a,
	int ia,
	const char *b,
	int ib,
	int n,
	size_t size
);

void _IRanges_memcpy_from_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nelt,
	const char *src,
	size_t src_nelt,
	size_t size
);

void _IRanges_memcpy_from_subset(
	const int *subset,
	int n,
	char *dest,
	size_t dest_nelt,
	const char *src,
	size_t src_nelt,
	size_t size
);

void _IRanges_memcpy_to_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nelt,
	const char *src,
	size_t src_nelt,
	size_t size
);

void _IRanges_memcpy_to_subset(
	const int *subset,
	int n,
	char *dest,
	size_t dest_nelt,
	const char *src,
	size_t src_nelt,
	size_t size
);

void _IRanges_charcpy_from_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_length,
	const char *src,
	int src_length,
	const int *lkup,
	int lkup_length
);

void _IRanges_charcpy_from_subset_with_lkup(
	const int *subset,
	int n,
	char *dest,
	int dest_length,
	const char *src,
	int src_length,
	const int *lkup,
	int lkup_length
);

void _IRanges_charcpy_to_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_length,
	const char *src,
	int src_length,
	const int *lkup,
	int lkup_length
);

void _IRanges_charcpy_to_subset_with_lkup(
	const int *subset,
	int n,
	char *dest,
	int dest_length,
	const char *src,
	int src_length,
	const int *lkup,
	int lkup_length
);

void _IRanges_reverse_memcpy_from_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nelt,
	const char *src,
	size_t src_nelt,
	size_t size
);

void _IRanges_reverse_charcpy_from_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_length,
	const char *src,
	int src_length,
	const int *lkup,
	int lkup_length
);

void _IRanges_memcpy_from_i1i2_to_complex(
	int i1,
	int i2,
	Rcomplex *dest,
	int dest_length,
	const char *src,
	int src_length,
	const Rcomplex *lkup,
	int lkup_length
);


/* SEXP_utils.c */

const char *_get_classname(SEXP x);

SEXP address_asSTRSXP(SEXP s);

SEXP listofvectors_lengths(SEXP x);

SEXP safe_strexplode(SEXP s);

SEXP Integer_diff_with_0(SEXP x);

SEXP Integer_sorted_merge(
	SEXP x,
	SEXP y
);

SEXP Integer_sorted_findInterval(
	SEXP x,
	SEXP vec
);


/* IRanges_class.c */

SEXP debug_IRanges_class();

SEXP _get_IRanges_start(SEXP x);

int _get_IRanges_length(SEXP x);

SEXP _get_IRanges_width(SEXP x);

SEXP _get_IRanges_names(SEXP x);

cachedIRanges _cache_subIRanges(
	SEXP x,
	int offset,
	int length
);

cachedIRanges _cache_IRanges(SEXP x);

int _get_cachedIRanges_length(const cachedIRanges *cached_x);

int _get_cachedIRanges_elt_start(
	const cachedIRanges *cached_x,
	int i
);

int _get_cachedIRanges_elt_width(
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

SEXP _alloc_IRanges(
	const char *classname,
	int length
);

SEXP IRanges_from_integer(SEXP x);


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


/* IRanges_utils.c */

SEXP debug_IRanges_utils();

SEXP which_as_IRanges(SEXP x);

SEXP IRanges_reduce(
	SEXP x,
	SEXP with_inframe_start
);


/* CompressedIRangesList_class.c */

cachedIRanges _cache_CompressedIRangesList_elt(
	SEXP x,
	int i
);

SEXP CompressedIRangesList_summary(SEXP object);


/* Grouping_class.c */

SEXP debug_Grouping_class();

SEXP _get_H2LGrouping_high2low(SEXP x);

SEXP _get_H2LGrouping_low2high(SEXP x);

SEXP H2LGrouping_members(
	SEXP x,
	SEXP group_ids
);

SEXP H2LGrouping_vmembers(
	SEXP x,
	SEXP group_ids_list
);


/* Sequence_class.c */

SEXP vector_subsetbyranges(SEXP x, SEXP start, SEXP width);


/* SequencePtr_class.c */

SEXP debug_SequencePtr_class();

SEXP ExternalPtr_show(SEXP xp);

SEXP ExternalPtr_new();

SEXP _new_SequencePtr(const char *classname, SEXP tag);

SEXP _get_SequencePtr_tag(SEXP x);

int _get_SequencePtr_length(SEXP x);

SEXP SequencePtr_length(SEXP x);


/* RawPtr_utils.c */

SEXP debug_RawPtr_utils();

SEXP RawPtr_new(
	SEXP length,
	SEXP val
);

SEXP RawPtr_get_show_string(SEXP x);

SEXP RawPtr_memcmp(
	SEXP x1,
	SEXP start1,
	SEXP x2,
	SEXP start2,
	SEXP width
);

SEXP RawPtr_memcpy(
	SEXP dest,
	SEXP dest_start,
	SEXP src,
	SEXP src_start,
	SEXP width
);

SEXP RawPtr_copy_from_i1i2(
	SEXP dest,
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP RawPtr_copy_from_subset(
	SEXP dest,
	SEXP src,
	SEXP subset
);

SEXP RawPtr_read_chars_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP RawPtr_read_chars_from_subset(
	SEXP src,
	SEXP subset
);

SEXP RawPtr_write_chars_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP string
);

SEXP RawPtr_write_chars_to_subset(
	SEXP dest,
	SEXP subset,
	SEXP string
);

SEXP RawPtr_read_ints_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP RawPtr_read_ints_from_subset(
	SEXP src,
	SEXP subset
);

SEXP RawPtr_write_ints_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP val
);

SEXP RawPtr_write_ints_to_subset(
	SEXP dest,
	SEXP subset,
	SEXP val
);

SEXP RawPtr_read_enc_chars_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax,
	SEXP lkup
);

SEXP RawPtr_read_enc_chars_from_subset(
	SEXP src,
	SEXP subset,
	SEXP lkup
);

SEXP RawPtr_write_enc_chars_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP string,
	SEXP lkup
);

SEXP RawPtr_write_enc_chars_to_subset(
	SEXP dest,
	SEXP subset,
	SEXP string,
	SEXP lkup
);

SEXP RawPtr_read_complexes_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax,
	SEXP lkup
);

SEXP RawPtr_read_complexes_from_subset(
	SEXP src,
	SEXP subset,
	SEXP lkup
);

SEXP RawPtr_translate_copy_from_i1i2(
	SEXP dest,
	SEXP src,
	SEXP imin,
	SEXP imax,
	SEXP lkup
);

SEXP RawPtr_translate_copy_from_subset(
	SEXP dest,
	SEXP src,
	SEXP subset,
	SEXP lkup
);

SEXP RawPtr_reverse_copy_from_i1i2(
	SEXP dest,
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP RawPtr_reverse_translate_copy_from_i1i2(
	SEXP dest,
	SEXP src,
	SEXP imin,
	SEXP imax,
	SEXP lkup
);


/* IntegerPtr_utils.c */

SEXP debug_IntegerPtr_utils();

SEXP IntegerPtr_new(
	SEXP length,
	SEXP val
);

SEXP IntegerPtr_get_show_string(SEXP x);

SEXP IntegerPtr_memcmp(
	SEXP x1,
	SEXP start1,
	SEXP x2,
	SEXP start2,
	SEXP width
);

SEXP IntegerPtr_copy_from_i1i2(
	SEXP dest,
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP IntegerPtr_copy_from_subset(
	SEXP dest,
	SEXP src,
	SEXP subset
);

SEXP IntegerPtr_read_ints_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP IntegerPtr_read_ints_from_subset(
	SEXP src,
	SEXP subset
);

SEXP IntegerPtr_write_ints_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP val
);

SEXP IntegerPtr_write_ints_to_subset(
	SEXP dest,
	SEXP subset,
	SEXP val
);


/* NumericPtr_utils.c */

SEXP debug_NumericPtr_utils();

SEXP NumericPtr_new(
	SEXP length,
	SEXP val
);

SEXP NumericPtr_get_show_string(SEXP x);

SEXP NumericPtr_memcmp(
	SEXP x1,
	SEXP start1,
	SEXP x2,
	SEXP start2,
	SEXP width
);

SEXP NumericPtr_read_nums_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP NumericPtr_read_nums_from_subset(
	SEXP src,
	SEXP subset
);

SEXP NumericPtr_write_nums_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP val
);

SEXP NumericPtr_write_nums_to_subset(
	SEXP dest,
	SEXP subset,
	SEXP val
);


/* XSequence_class.c */

SEXP _get_XSequence_xdata(SEXP x);

SEXP _get_XSequence_tag(SEXP x);

SEXP _get_XSequence_offset(SEXP x);

SEXP _get_XSequence_length(SEXP x);

SEXP _new_XSequence(
	const char *classname,
	SEXP xdata,
	int offset,
	int length
);

SEXP _new_XRaw_from_tag(
	const char *classname,
	SEXP tag
);

SEXP _new_XInteger_from_tag(
	const char *classname,
	SEXP tag
);

SEXP _new_XNumeric_from_tag(
	const char *classname,
	SEXP tag
);


/* XIntegerViews_class.c */

SEXP XIntegerViews_slice(
	SEXP xint,
	SEXP lower,
	SEXP upper
);


/* XIntegerViews_utils.c */

SEXP XIntegerViews_viewMins(
	SEXP x,
	SEXP na_rm
);

SEXP XIntegerViews_viewMaxs(
	SEXP x,
	SEXP na_rm
);

SEXP XIntegerViews_viewSums(
	SEXP x,
	SEXP na_rm
);

SEXP XIntegerViews_viewWhichMins(
	SEXP x,
	SEXP na_rm
);

SEXP XIntegerViews_viewWhichMaxs(
	SEXP x,
	SEXP na_rm
);


/* XNumericViews_class.c */

SEXP XNumericViews_slice(
	SEXP xdouble,
	SEXP lower,
	SEXP upper,
	SEXP include_lower,
	SEXP include_upper
);


/* Rle_class.c */

SEXP Rle_constructor(SEXP x, SEXP count);

SEXP Rle_run_seqblock(
	SEXP x,
	SEXP runStart,
	SEXP runEnd,
	SEXP offsetStart,
	SEXP offsetEnd,
	SEXP ans
);

SEXP Rle_seqblock(SEXP x, SEXP start, SEXP width);


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

SEXP RleViews_viewWhichMins(
	SEXP x,
	SEXP na_rm
);

SEXP RleViews_viewWhichMaxs(
	SEXP x,
	SEXP na_rm
);


/* coverage */

SEXP IRanges_coverage(
	SEXP x,
	SEXP weight,
	SEXP width
);

