#include "../inst/include/IRanges_defines.h"
#include <string.h>

#define DEBUG_IRANGES 1


/* utils.c */

SEXP debug_utils();

SEXP IRanges_length_vectors_in_list(SEXP list);

SEXP IRanges_sexp_address(SEXP s);

SEXP IRanges_xp_show(SEXP xp);

SEXP IRanges_xp_new();

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
	size_t dest_nmemb,
	const char *src,
	size_t src_nmemb,
	size_t size
);

void _IRanges_memcpy_from_subset(
	const int *subset,
	int n,
	char *dest,
	size_t dest_nmemb,
	const char *src,
	size_t src_nmemb,
	size_t size
);

void _IRanges_memcpy_to_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nmemb,
	const char *src,
	size_t src_nmemb,
	size_t size
);

void _IRanges_memcpy_to_subset(
	const int *subset,
	int n,
	char *dest,
	size_t dest_nmemb,
	const char *src,
	size_t src_nmemb,
	size_t size
);

/* AEbufs.c */

SEXP debug_AEbufs();

void _get_intorder(
	int len,
	const int *in,
	int *out
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


/* XInteger.c */

SEXP debug_XInteger();

SEXP XInteger_alloc(
	SEXP xint_xp,
	SEXP length
);

SEXP XInteger_get_show_string(SEXP xint_xp);

SEXP XInteger_length(SEXP xint_xp);

SEXP XInteger_memcmp(
	SEXP xint1_xp,
	SEXP start1,
	SEXP xint2_xp,
	SEXP start2,
	SEXP width
);

SEXP XInteger_read_ints_from_i1i2(
	SEXP src_xint_xp,
	SEXP imin,
	SEXP imax
);

SEXP XInteger_read_ints_from_subset(
	SEXP src_xint_xp,
	SEXP subset
);

SEXP XInteger_write_ints_to_i1i2(
	SEXP dest_xint_xp,
	SEXP imin,
	SEXP imax,
	SEXP val
);

SEXP XInteger_write_ints_to_subset(
	SEXP dest_xint_xp,
	SEXP subset,
	SEXP val
);


/* XNumeric.c */

SEXP debug_XNumeric();

SEXP XNumeric_alloc(
	SEXP xnum_xp,
	SEXP length
);

SEXP XNumeric_get_show_string(SEXP xnum_xp);

SEXP XNumeric_length(SEXP xnum_xp);

SEXP XNumeric_memcmp(
	SEXP xnum1_xp,
	SEXP start1,
	SEXP xnum2_xp,
	SEXP start2,
	SEXP width
);

SEXP XNumeric_read_nums_from_i1i2(
	SEXP src_xnum_xp,
	SEXP imin,
	SEXP imax
);

SEXP XNumeric_read_nums_from_subset(
	SEXP src_xnum_xp,
	SEXP subset
);

SEXP XNumeric_write_nums_to_i1i2(
	SEXP dest_xnum_xp,
	SEXP imin,
	SEXP imax,
	SEXP val
);

SEXP XNumeric_write_nums_to_subset(
	SEXP dest_xnum_xp,
	SEXP subset,
	SEXP val
);


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


/* IRanges_utils.c */

SEXP debug_IRanges_utils();

SEXP narrow_IRanges(
	SEXP x,
	SEXP start,
	SEXP end,
	SEXP width
);

SEXP int_to_adjacent_ranges(SEXP x);

SEXP which_as_ranges(SEXP x);

SEXP reduce_IRanges(
	SEXP x,
	SEXP with_inframe_start
);

SEXP IRanges_coverage(
	SEXP x,
	SEXP ans_length,
	SEXP weight
);

SEXP summary_IRanges_list(SEXP x);

