/*****************************************************************************
 IRanges C interface: prototypes
 -------------------------------

   The IRanges C interface is splitted in 2 files:
     1. IRanges_defines.h (in this directory): contains the typedefs and
        defines of the interface.
     2. IRanges_interface.h (this file): contains the prototypes of the
        IRanges C routines that are part of the interface.

 *****************************************************************************/
#include "IRanges_defines.h"


/*
 * Low-level sorting utilities.
 * (see sort_utils.c)
 */

void sort_int_array(
	int *x,
	int x_nelt
);

void get_int_array_order(
	const int *x,
	int x_nelt,
	int *order
);


/*
 * Low-level manipulation of the Auto-Extending buffers.
 * (see AEbufs.c)
 */

void IntAE_set_val(
	const IntAE *int_ae,
	int val
);

IntAE new_IntAE(
	int buflength,
	int nelt,
	int val
);

void IntAE_insert_at(
	IntAE *int_ae,
	int at,
	int val
);

void IntAE_append(
	IntAE *int_ae,
	const int *newvals,
	int nnewval
);

void IntAE_delete_at(
	IntAE *int_ae,
	int at
);

void IntAE_shift(
	const IntAE *int_ae,
	int shift
);

void IntAE_sum_and_shift(
	const IntAE *int_ae1,
	const IntAE *int_ae2,
	int shift
);

void IntAE_append_shifted_vals(
	IntAE *int_ae,
	const int *newvals,
	int nnewval,
	int shift
);

void IntAE_qsort(IntAE *int_ae);

void IntAE_delete_adjdups(IntAE *int_ae);

SEXP IntAE_asINTEGER(const IntAE *int_ae);

IntAE INTEGER_asIntAE(SEXP x);

IntAE CHARACTER_asIntAE(
	SEXP x,
	int keyshift
);

IntAEAE new_IntAEAE(
	int buflength,
	int nelt
);

void IntAEAE_insert_at(
	IntAEAE *int_aeae,
	int at,
	const IntAE *int_ae
);

void IntAEAE_eltwise_append(
	const IntAEAE *int_aeae1,
	const IntAEAE *int_aeae2
);

void IntAEAE_shift(
	const IntAEAE *int_aeae,
	int shift
);

void IntAEAE_sum_and_shift(
	const IntAEAE *int_aeae1,
	const IntAEAE *int_aeae2,
	int shift
);

SEXP IntAEAE_asLIST(
	const IntAEAE *int_aeae,
	int mode
);

IntAEAE LIST_asIntAEAE(SEXP x);

SEXP IntAEAE_toEnvir(
	const IntAEAE *int_aeae,
	SEXP envir,
	int keyshift
);

RangeAE new_RangeAE(
	int buflength,
	int nelt
);

void RangeAE_insert_at(
	RangeAE *range_ae,
	int at,
	int start,
	int width
);

SEXP RangeAE_asIRanges(const RangeAE *range_ae);

CharAE new_CharAE(int buflength);

CharAE new_CharAE_from_string(const char *string);

void CharAE_insert_at(
	CharAE *char_ae,
	int at,
	char c
);

void append_string_to_CharAE(
	CharAE *char_ae,
	const char *string
);

SEXP CharAE_asRAW(const CharAE *char_ae);

CharAEAE new_CharAEAE(
	int buflength,
	int nelt
);

void CharAEAE_insert_at(
	CharAEAE *char_aeae,
	int at,
	const CharAE *char_ae
);

void append_string_to_CharAEAE(
	CharAEAE *char_aeae,
	const char *string
);

SEXP CharAEAE_asCHARACTER(const CharAEAE *char_aeae);


/*
 * memcpy_utils.c
 */

int IRanges_memcmp(
	const char *a,
	int ia,
	const char *b,
	int ib,
	int n,
	size_t size
);

void IRanges_memcpy_from_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nelt,
	const char *src,
	size_t src_nelt,
	size_t size
);

void IRanges_memcpy_from_subset(
	const int *subset,
	int n,
	char *dest,
	size_t dest_nelt,
	const char *src,
	size_t src_nelt,
	size_t size
);

void IRanges_memcpy_to_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nelt,
	const char *src,
	size_t src_nelt,
	size_t size
);

void IRanges_memcpy_to_subset(
	const int *subset,
	int n,
	char *dest,
	size_t dest_nelt,
	const char *src,
	size_t src_nelt,
	size_t size
);

void IRanges_charcpy_from_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_length,
	const char *src,
	int src_length,
	const int *lkup,
	int lkup_length
);

void IRanges_charcpy_from_subset_with_lkup(
	const int *subset,
	int n,
	char *dest,
	int dest_length,
	const char *src,
	int src_length,
	const int *lkup,
	int lkup_length
);

void IRanges_charcpy_to_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_length,
	const char *src,
	int src_length,
	const int *lkup,
	int lkup_length
);

void IRanges_charcpy_to_subset_with_lkup(
	const int *subset,
	int n,
	char *dest,
	int dest_length,
	const char *src,
	int src_length,
	const int *lkup,
	int lkup_length
);

void IRanges_reverse_memcpy_from_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nelt,
	const char *src,
	size_t src_nelt,
	size_t size
);

void IRanges_reverse_charcpy_from_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_length,
	const char *src,
	int src_length,
	const int *lkup,
	int lkup_length
);

void IRanges_memcpy_from_i1i2_to_complex(
	int i1,
	int i2,
	Rcomplex *dest,
	int dest_length,
	const char *src,
	int src_length,
	const Rcomplex *lkup,
	int lkup_length
);


/*
 * SEXP_utils.c
 */
const char *get_classname(SEXP x);


/*
 * Low-level manipulation of IRanges objects.
 * (see IRanges_class.c)
 */

SEXP get_IRanges_start(SEXP x);

int get_IRanges_length(SEXP x);

SEXP get_IRanges_width(SEXP x);

SEXP get_IRanges_names(SEXP x);

cachedIRanges cache_IRanges(SEXP x);

int get_cachedIRanges_length(const cachedIRanges *cached_x);

int get_cachedIRanges_elt_start(const cachedIRanges *cached_x, int i);

int get_cachedIRanges_elt_width(const cachedIRanges *cached_x, int i);

int get_cachedIRanges_elt_end(const cachedIRanges *cached_x, int i);

SEXP get_cachedIRanges_elt_name(const cachedIRanges *cached_x, int i);

cachedIRanges sub_cachedIRanges(const cachedIRanges *cached_x, int offset, int length);

void set_IRanges_names(SEXP x, SEXP names);

void copy_IRanges_slots(SEXP x, SEXP x0);

SEXP new_IRanges(const char *classname, SEXP start, SEXP width, SEXP names);

SEXP alloc_IRanges(const char *classname, int length);


/*
 * Low-level manipulation of CompressedIRangesList objects.
 * (see CompressedIRangesList_class.c)
 */

cachedCompressedIRangesList cache_CompressedIRangesList(SEXP x);

cachedIRanges get_cachedCompressedIRangesList_elt(const cachedCompressedIRangesList *cached_x, int i);


/*
 * Low-level manipulation of Grouping objects.
 * (see Grouping_class.c)
 */

SEXP get_H2LGrouping_high2low(SEXP x);

SEXP get_H2LGrouping_low2high(SEXP x);


/*
 * Low-level manipulation of SequencePtr objects.
 * (see SequencePtr_class.c)
 */

SEXP new_SequencePtr(const char *classname, SEXP tag);

SEXP get_SequencePtr_tag(SEXP x);

int get_SequencePtr_length(SEXP x);


/*
 * Low-level manipulation of XSequence objects.
 * (see XSequence_class.c)
 */

SEXP get_XSequence_xdata(SEXP x);

SEXP get_XSequence_tag(SEXP x);

SEXP get_XSequence_offset(SEXP x);

SEXP get_XSequence_length(SEXP x);

SEXP new_XSequence(const char *classname, SEXP xdata, int offset, int length);

SEXP new_XRaw_from_tag(const char *classname, SEXP tag);

SEXP new_XInteger_from_tag(const char *classname, SEXP tag);

SEXP new_XNumeric_from_tag(const char *classname, SEXP tag);

