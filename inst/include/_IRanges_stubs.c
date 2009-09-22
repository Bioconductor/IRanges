#include "IRanges_interface.h"

#define DEFINE_CCALLABLE_STUB(retT, stubname, Targs, args) \
typedef retT(*__ ## stubname ## _funtype__)Targs; \
retT stubname Targs \
{ \
	static __ ## stubname ## _funtype__ fun = NULL; \
	if (fun == NULL) \
		fun = (__ ## stubname ## _funtype__) R_GetCCallable("IRanges", "_" #stubname); \
	return fun args; \
}

/*
 * Using the above macro when retT (the returned type) is void will make Sun
 * Studio 12 C compiler unhappy. So we need to use the following macro to
 * handle that case.
 */
#define DEFINE_NOVALUE_CCALLABLE_STUB(stubname, Targs, args) \
typedef void(*__ ## stubname ## _funtype__)Targs; \
void stubname Targs \
{ \
	static __ ## stubname ## _funtype__ fun = NULL; \
	if (fun == NULL) \
		fun = (__ ## stubname ## _funtype__) R_GetCCallable("IRanges", "_" #stubname); \
	fun args; \
	return; \
}


/*
 * Stubs for callables defined in sort_utils.c
 */

DEFINE_NOVALUE_CCALLABLE_STUB(sort_int_array,
	(int *x, int x_nelt),
	(     x,     x_nelt)
)

DEFINE_NOVALUE_CCALLABLE_STUB(get_int_array_order,
	(const int *x, int x_nelt, int *order),
	(           x,     x_nelt,      order)
)


/*
 * Stubs for callables defined in AEbufs.c
 */

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_set_val,
	(const IntAE *int_ae, int val),
	(             int_ae,     val)
)

DEFINE_CCALLABLE_STUB(IntAE, new_IntAE,
	(int buflength, int nelt, int val),
	(    buflength,     nelt,     val)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_insert_at,
	(IntAE *int_ae, int at, int val),
	(       int_ae,     at,     val)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_append,
	(IntAE *int_ae, const int *newvals, int nnewval),
	(       int_ae,            newvals,     nnewval)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_delete_at,
	(IntAE *int_ae, int at),
	(       int_ae,     at)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_shift,
	(const IntAE *int_ae, int shift),
	(             int_ae,     shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_sum_and_shift,
	(const IntAE *int_ae1, const IntAE *int_ae2, int shift),
	(             int_ae1,              int_ae2,     shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_append_shifted_vals,
	(IntAE *int_ae, const int *newvals, int nnewval, int shift),
	(       int_ae,            newvals,     nnewval,     shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_qsort,
	(IntAE *int_ae),
	(       int_ae)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAE_delete_adjdups,
	(IntAE *int_ae),
	(       int_ae)
)

DEFINE_CCALLABLE_STUB(SEXP, IntAE_asINTEGER,
	(const IntAE *int_ae),
	(             int_ae)
)

DEFINE_CCALLABLE_STUB(IntAE, INTEGER_asIntAE,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(IntAE, CHARACTER_asIntAE,
	(SEXP x, int keyshift),
	(     x,     keyshift)
)

DEFINE_CCALLABLE_STUB(IntAEAE, new_IntAEAE,
	(int buflength, int nelt),
	(    buflength,     nelt)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAEAE_insert_at,
	(IntAEAE *int_aeae, int at, const IntAE *int_ae),
	(         int_aeae,     at,              int_ae)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAEAE_eltwise_append,
	(const IntAEAE *int_aeae1, const IntAEAE *int_aeae2),
	(               int_aeae1,                int_aeae2)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAEAE_shift,
	(const IntAEAE *int_aeae, int shift),
	(               int_aeae,     shift)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IntAEAE_sum_and_shift,
	(const IntAEAE *int_aeae1, const IntAEAE *int_aeae2, int shift),
	(               int_aeae1,                int_aeae2,     shift)
)

DEFINE_CCALLABLE_STUB(SEXP, IntAEAE_asLIST,
	(const IntAEAE *int_aeae, int mode),
	(               int_aeae,     mode)
)

DEFINE_CCALLABLE_STUB(IntAEAE, LIST_asIntAEAE,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, IntAEAE_toEnvir,
	(const IntAEAE *int_aeae, SEXP envir, int keyshift),
	(               int_aeae,      envir,     keyshift)
)

DEFINE_CCALLABLE_STUB(RangeAE, new_RangeAE,
	(int buflength, int nelt),
	(    buflength,     nelt)
)

DEFINE_NOVALUE_CCALLABLE_STUB(RangeAE_insert_at,
	(RangeAE *range_ae, int at, int start, int width),
	(         range_ae,     at,     start,     width)
)

DEFINE_CCALLABLE_STUB(SEXP, RangeAE_asIRanges,
	(const RangeAE *range_ae),
	(               range_ae)
)

DEFINE_CCALLABLE_STUB(CharAE, new_CharAE,
	(int buflength),
	(    buflength)
)

DEFINE_CCALLABLE_STUB(CharAE, new_CharAE_from_string,
	(const char *string),
	(            string)
)

DEFINE_NOVALUE_CCALLABLE_STUB(CharAE_insert_at,
	(CharAE *char_ae, int at, char c),
	(        char_ae,     at,      c)
)

DEFINE_NOVALUE_CCALLABLE_STUB(append_string_to_CharAE,
	(CharAE *char_ae, const char *string),
	(        char_ae,             string)
)

DEFINE_CCALLABLE_STUB(SEXP, CharAE_asRAW,
	(const CharAE *char_ae),
	(              char_ae)
)

DEFINE_CCALLABLE_STUB(CharAEAE, new_CharAEAE,
	(int buflength, int nelt),
	(    buflength,     nelt)
)

DEFINE_NOVALUE_CCALLABLE_STUB(CharAEAE_insert_at,
	(CharAEAE *char_aeae, int at, const CharAE *char_ae),
	(          char_aeae,     at,               char_ae)
)

DEFINE_NOVALUE_CCALLABLE_STUB(append_string_to_CharAEAE,
	(CharAEAE *char_aeae, const char *string),
	(          char_aeae,             string)
)

DEFINE_CCALLABLE_STUB(SEXP, CharAEAE_asCHARACTER,
	(const CharAEAE *char_aeae),
	(                char_aeae)
)


/*
 * Stubs for callables defined in memcpy_utils.c
 */

DEFINE_CCALLABLE_STUB(int, IRanges_memcmp,
	(const char *a, int ia, const char *b, int ib, int n, size_t size),
	(            a,     ia,             b,     ib,     n,        size)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IRanges_memcpy_from_i1i2,
	(int i1, int i2, char *dest, size_t dest_nelt, const char *src, size_t src_nelt, size_t size),
	(    i1,     i2,       dest,        dest_nelt,             src,        src_nelt,        size)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IRanges_memcpy_from_subset,
	(const int *subset, int n, char *dest, size_t dest_nelt, const char *src, size_t src_nelt, size_t size),
	(           subset,     n,       dest,        dest_nelt,             src,        src_nelt,        size)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IRanges_memcpy_to_i1i2,
	(int i1, int i2, char *dest, size_t dest_nelt, const char *src, size_t src_nelt, size_t size),
	(    i1,     i2,       dest,        dest_nelt,             src,        src_nelt,        size)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IRanges_memcpy_to_subset,
	(const int *subset, int n, char *dest, size_t dest_nelt, const char *src, size_t src_nelt, size_t size),
	(           subset,     n,       dest,        dest_nelt,             src,        src_nelt,        size)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IRanges_charcpy_from_i1i2_with_lkup,
	(int i1, int i2, char *dest, int dest_length, const char *src, int src_length, const int *lkup, int lkup_length),
	(    i1,     i2,       dest,     dest_length,             src,     src_length,            lkup,     lkup_length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IRanges_charcpy_from_subset_with_lkup,
	(const int *subset, int n, char *dest, int dest_length, const char *src, int src_length, const int *lkup, int lkup_length),
	(           subset,     n,       dest,     dest_length,             src,     src_length,            lkup,     lkup_length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IRanges_charcpy_to_i1i2_with_lkup,
	(int i1, int i2, char *dest, int dest_length, const char *src, int src_length, const int *lkup, int lkup_length),
	(    i1,     i2,       dest,     dest_length,             src,     src_length,            lkup,     lkup_length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IRanges_charcpy_to_subset_with_lkup,
	(const int *subset, int n, char *dest, int dest_length, const char *src, int src_length, const int *lkup, int lkup_length),
	(           subset,     n,       dest,     dest_length,             src,     src_length,            lkup,     lkup_length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IRanges_reverse_memcpy_from_i1i2,
	(int i1, int i2, char *dest, size_t dest_nelt, const char *src, size_t src_nelt, size_t size),
	(    i1,     i2,       dest,        dest_nelt,             src,        src_nelt,        size)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IRanges_reverse_charcpy_from_i1i2_with_lkup,
	(int i1, int i2, char *dest, int dest_length, const char *src, int src_length, const int *lkup, int lkup_length),
	(    i1,     i2,       dest,     dest_length,             src,     src_length,            lkup,     lkup_length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(IRanges_memcpy_from_i1i2_to_complex,
	(int i1, int i2, Rcomplex *dest, int dest_length, const char *src, int src_length, const Rcomplex *lkup, int lkup_length),
	(    i1,     i2,           dest,     dest_length,             src,     src_length,                 lkup,     lkup_length)
)


/*
 * Stubs for callables defined in SEXP_utils.c
 */

DEFINE_CCALLABLE_STUB(const char *, get_classname,
	(SEXP x),
	(     x)
)

/*
 * Stubs for callables defined in Sequence_class.c
 */

DEFINE_CCALLABLE_STUB(const char *, get_Sequence_elementType,
	(SEXP x),
	(     x)
)

/*
 * Stubs for callables defined in IRanges_class.c
 */

DEFINE_CCALLABLE_STUB(SEXP, get_IRanges_start,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, get_IRanges_width,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, get_IRanges_names,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(int, get_IRanges_length,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(cachedIRanges, cache_IRanges,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(int, get_cachedIRanges_length,
	(const cachedIRanges *cached_x),
	(                     cached_x)
)

DEFINE_CCALLABLE_STUB(int, get_cachedIRanges_elt_width,
	(const cachedIRanges *cached_x, int i),
	(                     cached_x,     i)
)

DEFINE_CCALLABLE_STUB(int, get_cachedIRanges_elt_start,
	(const cachedIRanges *cached_x, int i),
	(                     cached_x,     i)
)

DEFINE_CCALLABLE_STUB(int, get_cachedIRanges_elt_end,
	(const cachedIRanges *cached_x, int i),
	(                     cached_x,     i)
)

DEFINE_CCALLABLE_STUB(SEXP, get_cachedIRanges_elt_name,
	(const cachedIRanges *cached_x, int i),
	(                     cached_x,     i)
)

DEFINE_CCALLABLE_STUB(cachedIRanges, sub_cachedIRanges,
	(const cachedIRanges *cached_x, int offset, int length),
	(                     cached_x,     offset,     length)
)

DEFINE_NOVALUE_CCALLABLE_STUB(set_IRanges_names,
	(SEXP x, SEXP names),
	(     x,      names)
)

DEFINE_NOVALUE_CCALLABLE_STUB(copy_IRanges_slots,
	(SEXP x, SEXP x0),
	(     x,      x0)
)

DEFINE_CCALLABLE_STUB(SEXP, new_IRanges,
	(const char *classname, SEXP start, SEXP width, SEXP names),
	(            classname,      start,      width,      names)
)

DEFINE_CCALLABLE_STUB(SEXP, alloc_IRanges,
	(const char *classname, int length),
	(            classname,     length)
)

/*
 * Stubs for callables defined in CompressedIRangesList_class.c
 */

DEFINE_CCALLABLE_STUB(cachedCompressedIRangesList, cache_CompressedIRangesList,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(cachedIRanges, get_cachedCompressedIRangesList_elt,
	(const cachedCompressedIRangesList *cached_x, int i),
	(                                   cached_x,     i)
)

/*
 * Stubs for callables defined in Grouping_class.c
 */

DEFINE_CCALLABLE_STUB(SEXP, get_H2LGrouping_high2low,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, get_H2LGrouping_low2high,
	(SEXP x),
	(     x)
)

/*
 * Stubs for callables defined in SharedVector_class.c
 */

DEFINE_CCALLABLE_STUB(SEXP, new_SharedVector,
	(const char *classname, SEXP tag),
	(            classname,      tag)
)

DEFINE_CCALLABLE_STUB(SEXP, get_SharedVector_tag,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(int, get_SharedVector_length,
	(SEXP x),
	(     x)
)

/*
 * Stubs for callables defined in XVector_class.c
 */

DEFINE_CCALLABLE_STUB(SEXP, get_XVector_shared,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(int, get_XVector_offset,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(int, get_XVector_length,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, get_XVector_tag,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(cachedCharSeq, cache_XRaw,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(cachedIntSeq, cache_XInteger,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(cachedDoubleSeq, cache_XDouble,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XVector,
	(const char *classname, SEXP shared, int offset, int length),
	(            classname,      shared,     offset,     length)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XRaw_from_tag,
	(const char *classname, SEXP tag),
	(            classname,      tag)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XInteger_from_tag,
	(const char *classname, SEXP tag),
	(            classname,      tag)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XDouble_from_tag,
	(const char *classname, SEXP tag),
	(            classname,      tag)
)

/*
 * Stubs for callables defined in XVectorList_class.c
 */

DEFINE_CCALLABLE_STUB(int, get_XVectorList_length,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, get_XVectorList_width,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(SEXP, get_XVectorList_names,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(cachedXVectorList, cache_XVectorList,
	(SEXP x),
	(     x)
)

DEFINE_CCALLABLE_STUB(int, get_cachedXVectorList_length,
	(const cachedXVectorList *cached_x),
	(                         cached_x)
)

DEFINE_CCALLABLE_STUB(cachedCharSeq, get_cachedXRawList_elt,
	(const cachedXVectorList *cached_x, int i),
	(                         cached_x,     i)
)

DEFINE_CCALLABLE_STUB(cachedIntSeq, get_cachedXIntegerList_elt,
	(const cachedXVectorList *cached_x, int i),
	(                         cached_x,     i)
)

DEFINE_CCALLABLE_STUB(cachedDoubleSeq, get_cachedXDoubleList_elt,
	(const cachedXVectorList *cached_x, int i),
	(                         cached_x,     i)
)

DEFINE_NOVALUE_CCALLABLE_STUB(set_XVectorList_names,
	(SEXP x, SEXP names),
	(     x,      names)
)

DEFINE_CCALLABLE_STUB(SEXP, new_XVectorList1,
	(const char *classname, SEXP xvector, SEXP ranges),
	(            classname,      xvector,      ranges)
)

