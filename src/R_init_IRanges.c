#include "IRanges.h"

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#define REGISTER_CCALLABLE(fun) \
	R_RegisterCCallable("IRanges", #fun, (DL_FUNC) &fun)

static const R_CallMethodDef callMethods[] = {

/* AEbufs.c */
	CALLMETHOD_DEF(debug_AEbufs, 0),

/* Ocopy_byteblocks.c */
	CALLMETHOD_DEF(debug_Ocopy_byteblocks, 0),

/* SEXP_utils.c */
	CALLMETHOD_DEF(address_asSTRSXP, 1),
	CALLMETHOD_DEF(listofvectors_lengths, 1),
	CALLMETHOD_DEF(Integer_diff_with_0, 1),
	CALLMETHOD_DEF(Integer_sorted_merge, 2),
	CALLMETHOD_DEF(findIntervalAndStartFromWidth, 2),

/* strutils.c */
	CALLMETHOD_DEF(safe_strexplode, 1),
	CALLMETHOD_DEF(strsplit_as_list_of_ints, 2),

/* Sequence_class.c */
	CALLMETHOD_DEF(vector_seqselect, 3),

/* IRanges_class.c */
	CALLMETHOD_DEF(debug_IRanges_class, 0),
	CALLMETHOD_DEF(IRanges_isNormal, 1),
	CALLMETHOD_DEF(IRanges_from_integer, 1),
	CALLMETHOD_DEF(NormalIRanges_from_logical, 1),

/* IRanges_constructor.c */
	CALLMETHOD_DEF(solve_user_SEW0, 3),
	CALLMETHOD_DEF(solve_user_SEW, 6),

/* IRanges_utils.c */
	CALLMETHOD_DEF(debug_IRanges_utils, 0),
	CALLMETHOD_DEF(IRanges_reduce, 4),
	CALLMETHOD_DEF(IRanges_gaps, 3),

/* coverage */
	CALLMETHOD_DEF(IRanges_coverage, 3),

/* Grouping_class.c */
	CALLMETHOD_DEF(debug_Grouping_class, 0),

	CALLMETHOD_DEF(H2LGrouping_members, 2),
	CALLMETHOD_DEF(H2LGrouping_vmembers, 2),

/* SimpleIRangesList_class.c */
	CALLMETHOD_DEF(SimpleIRangesList_isNormal, 1),
	CALLMETHOD_DEF(SimpleNormalIRangesList_min, 1),
	CALLMETHOD_DEF(SimpleNormalIRangesList_max, 1),

/* CompressedIRangesList_class.c */
	CALLMETHOD_DEF(CompressedIRangesList_isNormal, 2),
	CALLMETHOD_DEF(CompressedIRangesList_reduce, 3),
	CALLMETHOD_DEF(CompressedIRangesList_gaps, 3),
	CALLMETHOD_DEF(CompressedIRangesList_summary, 1),
	CALLMETHOD_DEF(CompressedNormalIRangesList_min, 2),
	CALLMETHOD_DEF(CompressedNormalIRangesList_max, 2),

/* GappedRanges_class.c */
	CALLMETHOD_DEF(valid_GappedRanges, 2),

/* Ranges_comparison.c */
	CALLMETHOD_DEF(Ranges_order, 3),

/* Rle_class.c */
	CALLMETHOD_DEF(Rle_constructor, 2),
	CALLMETHOD_DEF(Rle_start, 1),
	CALLMETHOD_DEF(Rle_end, 1),
	CALLMETHOD_DEF(Rle_window_aslist, 5),
	CALLMETHOD_DEF(Rle_window, 6),

/* Rle_utils.c */
	CALLMETHOD_DEF(Rle_runsum, 2),
	CALLMETHOD_DEF(Rle_runwtsum, 3),
	CALLMETHOD_DEF(Rle_runq, 3),

/* RleViews_utils.c */
	CALLMETHOD_DEF(RleViews_viewMins, 2),
	CALLMETHOD_DEF(RleViews_viewMaxs, 2),
	CALLMETHOD_DEF(RleViews_viewSums, 2),
	CALLMETHOD_DEF(RleViews_viewMeans, 2),
	CALLMETHOD_DEF(RleViews_viewWhichMins, 2),
	CALLMETHOD_DEF(RleViews_viewWhichMaxs, 2),

/* SharedVector_class.c */
	CALLMETHOD_DEF(debug_SharedVector_class, 0),
	CALLMETHOD_DEF(externalptr_tagtype, 1),
	CALLMETHOD_DEF(externalptr_taglength, 1),
	CALLMETHOD_DEF(externalptr_show, 1),
	CALLMETHOD_DEF(externalptr_new, 0),
	CALLMETHOD_DEF(SharedVector_memcmp, 5),
	CALLMETHOD_DEF(SharedVector_Ocopy_from_start, 6),
	CALLMETHOD_DEF(SharedVector_Ocopy_from_subscript, 4),
	CALLMETHOD_DEF(SharedVector_mcopy, 7),

/* SharedRaw_utils.c */
	CALLMETHOD_DEF(debug_SharedRaw_utils, 0),

	CALLMETHOD_DEF(SharedRaw_new, 2),
	CALLMETHOD_DEF(SharedRaw_address0, 1),

	CALLMETHOD_DEF(SharedRaw_read_chars_from_i1i2, 3),
	CALLMETHOD_DEF(SharedRaw_read_chars_from_subscript, 2),
	CALLMETHOD_DEF(SharedRaw_write_chars_to_i1i2, 4),
	CALLMETHOD_DEF(SharedRaw_write_chars_to_subscript, 3),

	CALLMETHOD_DEF(SharedRaw_read_ints_from_i1i2, 3),
	CALLMETHOD_DEF(SharedRaw_read_ints_from_subscript, 2),
	CALLMETHOD_DEF(SharedRaw_write_ints_to_i1i2, 4),
	CALLMETHOD_DEF(SharedRaw_write_ints_to_subscript, 3),

	CALLMETHOD_DEF(SharedRaw_read_enc_chars_from_i1i2, 4),
	CALLMETHOD_DEF(SharedRaw_read_enc_chars_from_subscript, 3),
	CALLMETHOD_DEF(SharedRaw_write_enc_chars_to_i1i2, 5),
	CALLMETHOD_DEF(SharedRaw_write_enc_chars_to_subscript, 4),

	CALLMETHOD_DEF(SharedRaw_read_complexes_from_i1i2, 4),
	CALLMETHOD_DEF(SharedRaw_read_complexes_from_subscript, 3),

/* SharedInteger_utils.c */
	CALLMETHOD_DEF(debug_SharedInteger_utils, 0),

	CALLMETHOD_DEF(SharedInteger_new, 2),
	CALLMETHOD_DEF(SharedInteger_get_show_string, 1),

	CALLMETHOD_DEF(SharedInteger_read_ints_from_i1i2, 3),
	CALLMETHOD_DEF(SharedInteger_read_ints_from_subscript, 2),
	CALLMETHOD_DEF(SharedInteger_write_ints_to_i1i2, 4),
	CALLMETHOD_DEF(SharedInteger_write_ints_to_subscript, 3),

/* SharedDouble_utils.c */
	CALLMETHOD_DEF(debug_SharedDouble_utils, 0),

	CALLMETHOD_DEF(SharedDouble_new, 2),
	CALLMETHOD_DEF(SharedDouble_get_show_string, 1),

	CALLMETHOD_DEF(SharedDouble_read_nums_from_i1i2, 3),
	CALLMETHOD_DEF(SharedDouble_read_nums_from_subscript, 2),
	CALLMETHOD_DEF(SharedDouble_write_nums_to_i1i2, 4),
	CALLMETHOD_DEF(SharedDouble_write_nums_to_subscript, 3),

/* XVector_class.c */
	CALLMETHOD_DEF(debug_XVector_class, 0),

/* XVectorList_class.c */
	CALLMETHOD_DEF(debug_XVectorList_class, 0),

/* XIntegerViews_class.c */
	CALLMETHOD_DEF(XIntegerViews_slice, 3),

/* XIntegerViews_utils.c */
	CALLMETHOD_DEF(XIntegerViews_viewMins, 2),
	CALLMETHOD_DEF(XIntegerViews_viewMaxs, 2),
	CALLMETHOD_DEF(XIntegerViews_viewSums, 2),
	CALLMETHOD_DEF(XIntegerViews_viewWhichMins, 2),
	CALLMETHOD_DEF(XIntegerViews_viewWhichMaxs, 2),

/* XDoubleViews_class.c */
	CALLMETHOD_DEF(XDoubleViews_slice, 5),

	{NULL, NULL, 0}
};


void R_init_IRanges(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);

/* sort_utils.c */
	REGISTER_CCALLABLE(_sort_int_array);
	REGISTER_CCALLABLE(_get_order_of_int_array);
	REGISTER_CCALLABLE(_get_order_of_two_int_arrays);

/* AEbufs.c */
	REGISTER_CCALLABLE(_get_new_buflength);
	REGISTER_CCALLABLE(_IntAE_set_val);
	REGISTER_CCALLABLE(_new_IntAE);
	REGISTER_CCALLABLE(_IntAE_insert_at);
	REGISTER_CCALLABLE(_IntAE_append);
	REGISTER_CCALLABLE(_IntAE_delete_at);
	REGISTER_CCALLABLE(_IntAE_shift);
	REGISTER_CCALLABLE(_IntAE_sum_and_shift);
	REGISTER_CCALLABLE(_IntAE_append_shifted_vals);
	REGISTER_CCALLABLE(_IntAE_qsort);
	REGISTER_CCALLABLE(_IntAE_delete_adjdups);
	REGISTER_CCALLABLE(_IntAE_asINTEGER);
	REGISTER_CCALLABLE(_INTEGER_asIntAE);
	REGISTER_CCALLABLE(_CHARACTER_asIntAE);
	REGISTER_CCALLABLE(_new_IntAEAE);
	REGISTER_CCALLABLE(_IntAEAE_insert_at);
	REGISTER_CCALLABLE(_IntAEAE_eltwise_append);
	REGISTER_CCALLABLE(_IntAEAE_shift);
	REGISTER_CCALLABLE(_IntAEAE_sum_and_shift);
	REGISTER_CCALLABLE(_IntAEAE_asLIST);
	REGISTER_CCALLABLE(_LIST_asIntAEAE);
	REGISTER_CCALLABLE(_IntAEAE_toEnvir);
	REGISTER_CCALLABLE(_new_RangeAE);
	REGISTER_CCALLABLE(_RangeAE_insert_at);
	REGISTER_CCALLABLE(_RangeAE_asIRanges);
	REGISTER_CCALLABLE(_new_RangeAEAE);
	REGISTER_CCALLABLE(_RangeAEAE_insert_at);
	REGISTER_CCALLABLE(_RangeAEAE_asLIST);
	REGISTER_CCALLABLE(_new_CharAE);
	REGISTER_CCALLABLE(_new_CharAE_from_string);
	REGISTER_CCALLABLE(_CharAE_insert_at);
	REGISTER_CCALLABLE(_append_string_to_CharAE);
	REGISTER_CCALLABLE(_CharAE_asRAW);
	REGISTER_CCALLABLE(_new_CharAEAE);
	REGISTER_CCALLABLE(_CharAEAE_insert_at);
	REGISTER_CCALLABLE(_append_string_to_CharAEAE);
	REGISTER_CCALLABLE(_CharAEAE_asCHARACTER);

/* Ocopy_byteblocks.c */
	REGISTER_CCALLABLE(_Ocopy_byteblocks_from_i1i2);
	REGISTER_CCALLABLE(_Ocopy_byteblocks_from_subscript);
	REGISTER_CCALLABLE(_Ocopy_byteblocks_to_i1i2);
	REGISTER_CCALLABLE(_Ocopy_byteblocks_to_subscript);
	REGISTER_CCALLABLE(_Ocopy_bytes_from_i1i2_with_lkup);
	REGISTER_CCALLABLE(_Ocopy_bytes_from_subscript_with_lkup);
	REGISTER_CCALLABLE(_Ocopy_bytes_to_i1i2_with_lkup);
	REGISTER_CCALLABLE(_Ocopy_bytes_to_subscript_with_lkup);
	REGISTER_CCALLABLE(_Orevcopy_byteblocks_from_i1i2);
	REGISTER_CCALLABLE(_Orevcopy_bytes_from_i1i2_with_lkup);
	REGISTER_CCALLABLE(_Ocopy_bytes_from_i1i2_to_complex);

/* SEXP_utils.c */
	REGISTER_CCALLABLE(_get_classname);

/* Sequence_class.c */
	REGISTER_CCALLABLE(_get_Sequence_elementType);

/* IRanges_class.c */
	REGISTER_CCALLABLE(_get_IRanges_start);
	REGISTER_CCALLABLE(_get_IRanges_width);
	REGISTER_CCALLABLE(_get_IRanges_names);
	REGISTER_CCALLABLE(_get_IRanges_length);
	REGISTER_CCALLABLE(_cache_IRanges);
	REGISTER_CCALLABLE(_get_cachedIRanges_length);
	REGISTER_CCALLABLE(_get_cachedIRanges_elt_width);
	REGISTER_CCALLABLE(_get_cachedIRanges_elt_start);
	REGISTER_CCALLABLE(_get_cachedIRanges_elt_end);
	REGISTER_CCALLABLE(_get_cachedIRanges_elt_name);
	REGISTER_CCALLABLE(_sub_cachedIRanges);
	REGISTER_CCALLABLE(_set_IRanges_names);
	REGISTER_CCALLABLE(_copy_IRanges_slots);
	REGISTER_CCALLABLE(_new_IRanges);
	REGISTER_CCALLABLE(_alloc_IRanges);

/* Grouping_class.c */
	REGISTER_CCALLABLE(_get_H2LGrouping_high2low);
	REGISTER_CCALLABLE(_get_H2LGrouping_low2high);
	REGISTER_CCALLABLE(_get_Partitioning_names);
	REGISTER_CCALLABLE(_get_PartitioningByEnd_end);
	REGISTER_CCALLABLE(_new_PartitioningByEnd);

/* CompressedIRangesList_class.c */
	REGISTER_CCALLABLE(_cache_CompressedIRangesList);
	REGISTER_CCALLABLE(_get_cachedCompressedIRangesList_elt);
	REGISTER_CCALLABLE(_new_CompressedIRangesList);

/* SharedVector_class.c */
	REGISTER_CCALLABLE(_new_SharedVector);
	REGISTER_CCALLABLE(_get_SharedVector_tag);
	REGISTER_CCALLABLE(_get_SharedVector_length);

/* SharedRaw_utils.c */
	REGISTER_CCALLABLE(_Ocopy_cachedCharSeq_to_SharedRaw_offset);

/* XVector_class.c */
	REGISTER_CCALLABLE(_get_XVector_shared);
	REGISTER_CCALLABLE(_get_XVector_offset);
	REGISTER_CCALLABLE(_get_XVector_length);
	REGISTER_CCALLABLE(_get_XVector_tag);
	REGISTER_CCALLABLE(_cache_XRaw);
	REGISTER_CCALLABLE(_cache_XInteger);
	REGISTER_CCALLABLE(_cache_XDouble);
	REGISTER_CCALLABLE(_new_XVector);
	REGISTER_CCALLABLE(_new_XRaw_from_tag);
	REGISTER_CCALLABLE(_new_XInteger_from_tag);
	REGISTER_CCALLABLE(_new_XDouble_from_tag);
	REGISTER_CCALLABLE(_alloc_XRaw);
	REGISTER_CCALLABLE(_alloc_XInteger);
	REGISTER_CCALLABLE(_alloc_XDouble);

/* XVectorList_class.c */
	REGISTER_CCALLABLE(_get_XVectorList_length);
	REGISTER_CCALLABLE(_get_XVectorList_width);
	REGISTER_CCALLABLE(_get_XVectorList_names);
	REGISTER_CCALLABLE(_cache_XVectorList);
	REGISTER_CCALLABLE(_get_cachedXVectorList_length);
	REGISTER_CCALLABLE(_get_cachedXRawList_elt);
	REGISTER_CCALLABLE(_get_cachedXIntegerList_elt);
	REGISTER_CCALLABLE(_get_cachedXDoubleList_elt);
	REGISTER_CCALLABLE(_set_XVectorList_names);
	REGISTER_CCALLABLE(_new_XVectorList1);
	REGISTER_CCALLABLE(_alloc_XRawList);
	REGISTER_CCALLABLE(_alloc_XIntegerList);
	REGISTER_CCALLABLE(_alloc_XDoubleList);
	return;
}

