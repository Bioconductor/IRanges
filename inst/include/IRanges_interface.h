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
	int nelt,
	int desc
);

void get_order_of_int_array(
	const int *x,
	int nelt,
	int desc,
	int *out,
	int out_shift
);

void get_order_of_int_pairs(
	const int *a,
	const int *b,
	int nelt,
	int desc,
	int *out,
	int out_shift
);

void get_order_of_int_quads(
	const int *a,
	const int *b,
	const int *c,
	const int *d,
	int nelt,
	int desc,
	int *out,
	int out_shift
);

/*
 * Hash table management.
 * (see hash_utils.c)
 */

struct htab new_htab(int n);

int get_hbucket_val(
	const struct htab *htab,
	int bucket_idx
);

void set_hbucket_val(
	struct htab *htab,
	int bucket_idx,
	int val
);

/*
 * Low-level manipulation of the Auto-Extending buffers.
 * (see AEbufs.c)
 */

int get_new_buflength(int buflength);

int IntAE_get_nelt(const IntAE *int_ae);

int IntAE_set_nelt(
	IntAE *int_ae,
	int nelt
);

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

void IntAE_qsort(
	const IntAE *int_ae,
	int desc
);

void IntAE_delete_adjdups(IntAE *int_ae);

SEXP new_INTEGER_from_IntAE(const IntAE *int_ae);

IntAE new_IntAE_from_INTEGER(SEXP x);

IntAE new_IntAE_from_CHARACTER(
	SEXP x,
	int keyshift
);

int IntAEAE_get_nelt(const IntAEAE *int_aeae);

int IntAEAE_set_nelt(
	IntAEAE *int_aeae,
	int nelt
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

SEXP new_LIST_from_IntAEAE(
	const IntAEAE *int_aeae,
	int mode
);

IntAEAE new_IntAEAE_from_LIST(SEXP x);

SEXP IntAEAE_toEnvir(
	const IntAEAE *int_aeae,
	SEXP envir,
	int keyshift
);

int RangeAE_get_nelt(const RangeAE *range_ae);

int RangeAE_set_nelt(
	RangeAE *range_ae,
	int nelt
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

int RangeAEAE_get_nelt(const RangeAEAE *range_aeae);

int RangeAEAE_set_nelt(
	RangeAEAE *range_aeae,
	int nelt
);

RangeAEAE new_RangeAEAE(
	int buflength,
	int nelt
);

void RangeAEAE_insert_at(
	RangeAEAE *range_aeae,
	int at,
	const RangeAE *range_ae
);

int CharAE_get_nelt(const CharAE *char_ae);

int CharAE_set_nelt(
	CharAE *char_ae,
	int nelt
);

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

void CharAE_delete_at(
	CharAE *char_ae,
	int at,
	int nelt
);

SEXP new_RAW_from_CharAE(const CharAE *char_ae);

SEXP new_LOGICAL_from_CharAE(const CharAE *char_ae);

int CharAEAE_get_nelt(const CharAEAE *char_aeae);

int CharAEAE_set_nelt(
	CharAEAE *char_aeae,
	int nelt
);

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

SEXP new_CHARACTER_from_CharAEAE(const CharAEAE *char_aeae);

/*
 * SEXP_utils.c
 */

const char *get_classname(SEXP x);

/*
 * int_utils.c
 */

int check_integer_pairs(
	SEXP a,
	SEXP b,
	const int **a_p,
	const int **b_p,
	const char *a_argname,
	const char *b_argname
);

/*
 * Low-level manipulation of Vector objects.
 * (see Vector_class.c)
 */

const char *get_List_elementType(SEXP x);

void set_List_elementType(SEXP x, const char *type);

int vector_memcmp(
	SEXP x1,
	int x1_offset,
	SEXP x2,
	int x2_offset,
	int nelt
);

void vector_memcpy(
	SEXP out,
	int out_offset,
	SEXP in,
	int in_offset,
	int nelt
);

/*
 * Low-level manipulation of IRanges objects.
 * (see IRanges_class.c)
 */

SEXP get_IRanges_start(SEXP x);

SEXP get_IRanges_width(SEXP x);

SEXP get_IRanges_names(SEXP x);

int get_IRanges_length(SEXP x);

cachedIRanges cache_IRanges(SEXP x);

int get_cachedIRanges_length(const cachedIRanges *cached_x);

int get_cachedIRanges_elt_width(const cachedIRanges *cached_x, int i);

int get_cachedIRanges_elt_start(const cachedIRanges *cached_x, int i);

int get_cachedIRanges_elt_end(const cachedIRanges *cached_x, int i);

SEXP get_cachedIRanges_elt_name(const cachedIRanges *cached_x, int i);

cachedIRanges sub_cachedIRanges(const cachedIRanges *cached_x, int offset, int length);

void set_IRanges_names(SEXP x, SEXP names);

void copy_IRanges_slots(SEXP x, SEXP x0);

SEXP new_IRanges(const char *classname, SEXP start, SEXP width, SEXP names);

SEXP new_IRanges_from_RangeAE(const char *classname, const RangeAE *range_ae);

SEXP new_list_of_IRanges_from_RangeAEAE(const char *element_type, const RangeAEAE *range_aeae);

SEXP alloc_IRanges(const char *classname, int length);

/*
 * Low-level manipulation of Grouping objects.
 * (see Grouping_class.c)
 */

SEXP get_H2LGrouping_high2low(SEXP x);

SEXP get_H2LGrouping_low2high(SEXP x);

SEXP get_Partitioning_names(SEXP x);

SEXP get_PartitioningByEnd_end(SEXP x);

SEXP new_PartitioningByEnd(const char *classname, SEXP end, SEXP names);

/*
 * Low-level manipulation of SimpleList objects.
 * (see SimpleList_class.c)
 */

SEXP new_SimpleList(const char *classname, SEXP listData);

/*
 * Low-level manipulation of DataFrame objects.
 * (see DataFrame_class.c)
 */

SEXP new_DataFrame(const char *classname, SEXP vars, SEXP rownames, SEXP nrows);

/*
 * Low-level manipulation of CompressedList objects.
 * (see CompressedList_class.c)
 */

SEXP new_CompressedList(const char *classname, SEXP unlistData, SEXP partitioning);

/*
 * Low-level manipulation of CompressedIRangesList objects.
 * (see CompressedIRangesList_class.c)
 */

cachedCompressedIRangesList cache_CompressedIRangesList(SEXP x);

cachedIRanges get_cachedCompressedIRangesList_elt(const cachedCompressedIRangesList *cached_x, int i);

/*
 * Low-level manipulation of RangedData objects.
 * (see RangedData_class.c)
 */

SEXP new_RangedData(const char *classname, SEXP ranges, SEXP values);

/*
 * Low-level manipulation of Rle objects.
 * (see Rle_class.c)
 */

SEXP seqselect_Rle(SEXP x, const int *start, const int *width, int length);

