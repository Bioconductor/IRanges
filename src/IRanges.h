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

void _IntAE_sum_val(
	const IntAE *int_ae,
	int val
);

void _IntAE_append_shifted_vals(
	IntAE *int_ae,
	const int *newvals,
	int nnewval,
	int shift
);

void _IntAE_sum_IntAE(
	const IntAE *int_ae1,
	const IntAE *int_ae2
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

void _IntAEAE_sum_val(
	const IntAEAE *int_aeae,
	int val
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

const char *_get_class(SEXP x);

SEXP address_asSTRSXP(SEXP s);

SEXP sapply_length(SEXP list);

SEXP safe_strexplode(SEXP s);


/* IRanges_class.c */

SEXP debug_IRanges_class();

SEXP _get_IRanges_start(SEXP x);

SEXP _get_IRanges_width(SEXP x);

int _get_IRanges_length(SEXP x);

const int *_get_IRanges_start0(SEXP x);

const int *_get_IRanges_width0(SEXP x);

void _set_IRanges_names(
	SEXP x,
	SEXP names
);

void _copy_IRanges_slots(
	SEXP x,
	SEXP x0
);

SEXP _new_IRanges(
	const char *class,
	SEXP start,
	SEXP width,
	SEXP names
);

SEXP _alloc_IRanges(
	const char *class,
	int length
);


/* solve_user_SEW.c */

SEXP solve_user_SEW(
	SEXP refwidths,
	SEXP start,
	SEXP end,
	SEXP width,
	SEXP translate_nonpositive_coord,
	SEXP allow_nonnarrowing
);


/* IRanges_utils.c */

SEXP debug_IRanges_utils();

SEXP which_as_IRanges(SEXP x);

SEXP IRanges_reduce(
	SEXP x,
	SEXP with_inframe_start
);


/* RangesList_class.c */

SEXP summary_IRangesList(SEXP object);


/* SequencePtr_class.c */

SEXP debug_SequencePtr_class();

SEXP ExternalPtr_show(SEXP xp);

SEXP ExternalPtr_new();

SEXP _new_SequencePtr(const char *class, SEXP tag);

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


/* XNumericViews_class.c */

SEXP XNumericViews_slice(
	SEXP xdouble,
	SEXP lower,
	SEXP upper,
	SEXP include_lower,
	SEXP include_upper
);


/* XRleInteger_class.c */

SEXP XRleInteger_Arith(
	SEXP x,
	SEXP y,
	SEXP Generic
);


/* XRleIntegerViews_class.c */

SEXP XRleIntegerViews_slice(
	SEXP x,
	SEXP lower,
	SEXP upper
);


/* XRleIntegerViews_utils.c */

SEXP XRleIntegerViews_viewMins(
	SEXP x,
	SEXP na_rm
);

SEXP XRleIntegerViews_viewMaxs(
	SEXP x,
	SEXP na_rm
);

SEXP XRleIntegerViews_viewSums(
	SEXP x,
	SEXP na_rm
);


/* coverage */

SEXP IRanges_coverage(
	SEXP x,
	SEXP weight,
	SEXP order,
	SEXP width
);
