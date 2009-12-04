#include "../inst/include/IRanges_defines.h"
#include <string.h>

#define DEBUG_IRANGES 1

#define INIT_STATIC_SYMBOL(NAME) \
{ \
	if (NAME ## _symbol == NULL) \
		NAME ## _symbol = install(# NAME); \
}


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

int _get_new_buflength(int buflength);

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

RangeAEAE _new_RangeAEAE(
	int buflength,
	int nelt
);

void _RangeAEAE_insert_at(
	RangeAEAE *range_aeae,
	int at,
	const RangeAE *range_ae
);

SEXP _RangeAEAE_asLIST(const RangeAEAE *range_aeae);

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


/* Ocopy_byteblocks.c */

SEXP debug_Ocopy_byteblocks();

void _Ocopy_byteblocks_from_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void _Ocopy_byteblocks_from_subscript(
	const int *subscript,
	int n,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void _Ocopy_byteblocks_to_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void _Ocopy_byteblocks_to_subscript(
	const int *subscript,
	int n,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void _Ocopy_bytes_from_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void _Ocopy_bytes_from_subscript_with_lkup(
	const int *subscript,
	int n,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void _Ocopy_bytes_to_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void _Ocopy_bytes_to_subscript_with_lkup(
	const int *subscript,
	int n,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void _Orevcopy_byteblocks_from_i1i2(
	int i1,
	int i2,
	char *dest,
	size_t dest_nblocks,
	const char *src,
	size_t src_nblocks,
	size_t blocksize
);

void _Orevcopy_bytes_from_i1i2_with_lkup(
	int i1,
	int i2,
	char *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const int *lkup,
	int lkup_length
);

void _Ocopy_bytes_from_i1i2_to_complex(
	int i1,
	int i2,
	Rcomplex *dest,
	int dest_nbytes,
	const char *src,
	int src_nbytes,
	const Rcomplex *lkup,
	int lkup_length
);


/* vector_copy.c */

int _vector_memcmp(
	SEXP x1,
	int x1_offset,
	SEXP x2,
	int x2_offset,
	int nelt
);

void _vector_Ocopy(
	SEXP out,
	int out_offset,
	SEXP in,
	int in_offset,
	int nelt,
	SEXP lkup,
	int reverse,
	int Omode
);

void _vector_Ocopy_from_offset(
	SEXP out,
	SEXP in,
	int in_offset,
	int nelt,
	SEXP lkup,
	int reverse
);

void _vector_Ocopy_to_offset(
	SEXP out,
	SEXP in,
	int out_offset,
	int nelt,
	SEXP lkup
);

void _vector_Ocopy_from_subscript(
	SEXP out,
	SEXP in,
	SEXP subscript,
	SEXP lkup
);

void _vector_Ocopy_to_subscript(
	SEXP out,
	SEXP in,
	SEXP subscript,
	SEXP lkup
);

void _vector_mcopy(
	SEXP out,
	int out_offset,
	SEXP in,
	SEXP in_start,
	SEXP in_width,
	SEXP lkup,
	int reverse
);


/* SEXP_utils.c */

const char *_get_classname(SEXP x);

SEXP address_asSTRSXP(SEXP s);

SEXP listofvectors_lengths(SEXP x);

SEXP Integer_diff_with_0(SEXP x);

SEXP Integer_sorted_merge(
	SEXP x,
	SEXP y
);

SEXP findIntervalAndStartFromWidth(
	SEXP x,
	SEXP vec
);


/* strutils.c */

SEXP safe_strexplode(SEXP s);

SEXP strsplit_as_list_of_ints(SEXP x, SEXP sep);


/* Sequence_class.c */

const char *_get_Sequence_elementType(SEXP x);

void _set_Sequence_elementType(
	SEXP x,
	const char *type
);

SEXP vector_seqselect(
	SEXP x,
	SEXP start,
	SEXP width
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

SEXP _alloc_IRanges(
	const char *classname,
	int length
);

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


/* IRanges_utils.c */

SEXP debug_IRanges_utils();

SEXP IRanges_reduce(
	SEXP x,
	SEXP with_inframe_start
);


/* coverage */

SEXP IRanges_coverage(
	SEXP x,
	SEXP weight,
	SEXP width
);


/* Grouping_class.c */

SEXP debug_Grouping_class();

SEXP _get_H2LGrouping_high2low(SEXP x);

SEXP _get_H2LGrouping_low2high(SEXP x);

SEXP _get_PartitioningByEnd_end(SEXP x);

SEXP H2LGrouping_members(
	SEXP x,
	SEXP group_ids
);

SEXP H2LGrouping_vmembers(
	SEXP x,
	SEXP group_ids_list
);


/* CompressedIRangesList_class.c */

SEXP _get_CompressedIRangesList_unlistData(SEXP x);

SEXP _get_CompressedIRangesList_partitioning(SEXP x);

int _get_CompressedIRangesList_length(SEXP x);

cachedCompressedIRangesList _cache_CompressedIRangesList(SEXP x);

int _get_cachedCompressedIRangesList_length(
	const cachedCompressedIRangesList *cached_x
);

cachedIRanges _get_cachedCompressedIRangesList_elt(
	const cachedCompressedIRangesList *cached_x,
	int i
);

SEXP CompressedIRangesList_summary(SEXP object);


/* GappedRanges_class.c */

SEXP GappedRanges_start(SEXP x);

SEXP GappedRanges_end(SEXP x);


/* Ranges_comparison.c */

void _get_Ranges_order(
	int *order,
	int nelt,
	const int *start,
	const int *width,
	int decreasing,
	int base1
);

SEXP Ranges_order(
	SEXP start,
	SEXP width,
	SEXP decreasing
);


/* Rle_class.c */

SEXP Rle_constructor(
	SEXP x,
	SEXP count
);

SEXP Rle_start(SEXP x);

SEXP Rle_end(SEXP x);

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


/* Rle_utils.c */
SEXP Rle_runsum(
	SEXP x,
	SEXP k
);

SEXP Rle_runwtsum(
	SEXP x,
	SEXP k,
	SEXP wt
);

SEXP Rle_runq(
	SEXP x,
	SEXP k,
	SEXP which
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


/* SharedVector_class.c */

SEXP debug_SharedVector_class();

SEXP externalptr_tagtype(SEXP x);

SEXP externalptr_taglength(SEXP x);

SEXP externalptr_show(SEXP x);

SEXP externalptr_new();

SEXP _get_SharedVector_tag(SEXP x);

int _get_SharedVector_length(SEXP x);

SEXP _new_SharedVector(const char *classname, SEXP tag);

SEXP SharedVector_memcmp(
	SEXP x1,
	SEXP start1,
	SEXP x2,
	SEXP start2,
	SEXP width
);

SEXP SharedVector_Ocopy_from_start(
	SEXP out,
	SEXP in,
	SEXP in_start,
	SEXP width,
	SEXP lkup,
	SEXP reverse
);

SEXP SharedVector_Ocopy_from_subscript(
	SEXP out,
	SEXP in,
	SEXP subscript,
	SEXP lkup
);

SEXP SharedVector_mcopy(
	SEXP out,
	SEXP out_offset,
	SEXP in,
	SEXP in_start,
	SEXP in_width,
	SEXP lkup,
	SEXP reverse
);

SEXP _get_SharedVector_Pool_xp_list(SEXP x);

SEXP _new_SharedVector_Pool1(SEXP shared);


/* SharedRaw_utils.c */

SEXP debug_SharedRaw_utils();

SEXP SharedRaw_new(
	SEXP length,
	SEXP val
);

SEXP SharedRaw_address0(SEXP x);

SEXP SharedRaw_read_chars_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP SharedRaw_read_chars_from_subscript(
	SEXP src,
	SEXP subscript
);

SEXP SharedRaw_write_chars_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP string
);

SEXP SharedRaw_write_chars_to_subscript(
	SEXP dest,
	SEXP subscript,
	SEXP string
);

SEXP SharedRaw_read_ints_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP SharedRaw_read_ints_from_subscript(
	SEXP src,
	SEXP subscript
);

SEXP SharedRaw_write_ints_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP val
);

SEXP SharedRaw_write_ints_to_subscript(
	SEXP dest,
	SEXP subscript,
	SEXP val
);

SEXP SharedRaw_read_enc_chars_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax,
	SEXP lkup
);

SEXP SharedRaw_read_enc_chars_from_subscript(
	SEXP src,
	SEXP subscript,
	SEXP lkup
);

SEXP SharedRaw_write_enc_chars_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP string,
	SEXP lkup
);

SEXP SharedRaw_write_enc_chars_to_subscript(
	SEXP dest,
	SEXP subscript,
	SEXP string,
	SEXP lkup
);

SEXP SharedRaw_read_complexes_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax,
	SEXP lkup
);

SEXP SharedRaw_read_complexes_from_subscript(
	SEXP src,
	SEXP subscript,
	SEXP lkup
);

void _Ocopy_cachedCharSeq_to_SharedRaw_offset(
	SEXP out,
	int out_offset,
	const cachedCharSeq *in,
	const int *lkup,
	int lkup_length
);


/* SharedInteger_utils.c */

SEXP debug_SharedInteger_utils();

SEXP SharedInteger_new(
	SEXP length,
	SEXP val
);

SEXP SharedInteger_get_show_string(SEXP x);

SEXP SharedInteger_read_ints_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP SharedInteger_read_ints_from_subscript(
	SEXP src,
	SEXP subscript
);

SEXP SharedInteger_write_ints_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP val
);

SEXP SharedInteger_write_ints_to_subscript(
	SEXP dest,
	SEXP subscript,
	SEXP val
);


/* SharedDouble_utils.c */

SEXP debug_SharedDouble_utils();

SEXP SharedDouble_new(
	SEXP length,
	SEXP val
);

SEXP SharedDouble_get_show_string(SEXP x);

SEXP SharedDouble_read_nums_from_i1i2(
	SEXP src,
	SEXP imin,
	SEXP imax
);

SEXP SharedDouble_read_nums_from_subscript(
	SEXP src,
	SEXP subscript
);

SEXP SharedDouble_write_nums_to_i1i2(
	SEXP dest,
	SEXP imin,
	SEXP imax,
	SEXP val
);

SEXP SharedDouble_write_nums_to_subscript(
	SEXP dest,
	SEXP subscript,
	SEXP val
);


/* XVector_class.c */

SEXP debug_XVector_class();

SEXP _get_XVector_shared(SEXP x);

int _get_XVector_offset(SEXP x);

int _get_XVector_length(SEXP x);

SEXP _get_XVector_tag(SEXP x);

cachedCharSeq _cache_XRaw(SEXP x);

cachedIntSeq _cache_XInteger(SEXP x);

cachedDoubleSeq _cache_XDouble(SEXP x);

SEXP _new_XVector(
	const char *classname,
	SEXP shared,
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

SEXP _new_XDouble_from_tag(
	const char *classname,
	SEXP tag
);

SEXP _alloc_XRaw(
	const char *classname,
	int length
);

SEXP _alloc_XInteger(
	const char *classname,
	int length
);

SEXP _alloc_XDouble(
	const char *classname,
	int length
);


/* XVectorList_class.c */

SEXP debug_XVectorList_class();

SEXP _get_XVectorList_pool(SEXP x);

SEXP _get_XVectorList_ranges(SEXP x);

int _get_XVectorList_length(SEXP x);

SEXP _get_XVectorList_width(SEXP x);

SEXP _get_XVectorList_names(SEXP x);

cachedXVectorList _cache_XVectorList(SEXP x);

int _get_cachedXVectorList_length(const cachedXVectorList *cached_x);

cachedCharSeq _get_cachedXRawList_elt(
	const cachedXVectorList *cached_x,
	int i
);

cachedIntSeq _get_cachedXIntegerList_elt(
	const cachedXVectorList *cached_x,
	int i
);

cachedDoubleSeq _get_cachedXDoubleList_elt(
	const cachedXVectorList *cached_x,
	int i
);

void _set_XVectorList_names(SEXP x, SEXP names);

SEXP _new_XVectorList1(
	const char *classname,
	SEXP xvector,
	SEXP ranges
);

SEXP _alloc_XRawList(
	const char *classname,
	const char *element_type,
	SEXP width
);

SEXP _alloc_XIntegerList(
	const char *classname,
	const char *element_type,
	SEXP width
);

SEXP _alloc_XDoubleList(
	const char *classname,
	const char *element_type,
	SEXP width
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


/* XDoubleViews_class.c */

SEXP XDoubleViews_slice(
	SEXP xdouble,
	SEXP lower,
	SEXP upper,
	SEXP include_lower,
	SEXP include_upper
);
