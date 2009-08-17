#include "IRanges.h"

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#define REGISTER_CCALLABLE(fun) \
	R_RegisterCCallable("IRanges", #fun, (DL_FUNC) &fun)

static const R_CallMethodDef callMethods[] = {

/* AEbufs.c */
	CALLMETHOD_DEF(debug_AEbufs, 0),

/* memcpy_utils.c */
	CALLMETHOD_DEF(debug_memcpy_utils, 0),

/* SEXP_utils.c */
	CALLMETHOD_DEF(address_asSTRSXP, 1),
	CALLMETHOD_DEF(listofvectors_lengths, 1),
	CALLMETHOD_DEF(safe_strexplode, 1),
	CALLMETHOD_DEF(Integer_diff_with_0, 1),
	CALLMETHOD_DEF(Integer_sorted_merge, 2),
	CALLMETHOD_DEF(Integer_sorted_findInterval, 2),

/* IRanges_class.c */
	CALLMETHOD_DEF(debug_IRanges_class, 0),
	CALLMETHOD_DEF(IRanges_from_integer, 1),
	CALLMETHOD_DEF(NormalIRanges_from_logical, 1),

/* IRanges_constructor.c */
	CALLMETHOD_DEF(solve_user_SEW0, 3),
	CALLMETHOD_DEF(solve_user_SEW, 6),

/* IRanges_utils.c */
	CALLMETHOD_DEF(debug_IRanges_utils, 0),

	CALLMETHOD_DEF(IRanges_reduce, 2),

/* CompressedIRangesList_class.c */
	CALLMETHOD_DEF(CompressedIRangesList_summary, 1),

/* Grouping_class.c */
	CALLMETHOD_DEF(debug_Grouping_class, 0),

	CALLMETHOD_DEF(H2LGrouping_members, 2),
	CALLMETHOD_DEF(H2LGrouping_vmembers, 2),

/* Ranges_comparison.c */
	CALLMETHOD_DEF(Ranges_order, 3),

/* Sequence_class.c */
	CALLMETHOD_DEF(vector_subsetbyranges, 3),

/* SequencePtr_class.c */
	CALLMETHOD_DEF(debug_SequencePtr_class, 0),
	CALLMETHOD_DEF(ExternalPtr_show, 1),
	CALLMETHOD_DEF(ExternalPtr_new, 0),
	CALLMETHOD_DEF(SequencePtr_length, 1),

/* RawPtr_utils.c */
	CALLMETHOD_DEF(debug_RawPtr_utils, 0),

	CALLMETHOD_DEF(RawPtr_new, 2),
	CALLMETHOD_DEF(RawPtr_get_show_string, 1),
	CALLMETHOD_DEF(RawPtr_memcmp, 5),

	CALLMETHOD_DEF(RawPtr_memcpy, 5),
	CALLMETHOD_DEF(RawPtr_copy_from_i1i2, 4),
	CALLMETHOD_DEF(RawPtr_copy_from_subset, 3),

	CALLMETHOD_DEF(RawPtr_read_chars_from_i1i2, 3),
	CALLMETHOD_DEF(RawPtr_read_chars_from_subset, 2),
	CALLMETHOD_DEF(RawPtr_write_chars_to_i1i2, 4),
	CALLMETHOD_DEF(RawPtr_write_chars_to_subset, 3),

	CALLMETHOD_DEF(RawPtr_read_ints_from_i1i2, 3),
	CALLMETHOD_DEF(RawPtr_read_ints_from_subset, 2),
	CALLMETHOD_DEF(RawPtr_write_ints_to_i1i2, 4),
	CALLMETHOD_DEF(RawPtr_write_ints_to_subset, 3),

	CALLMETHOD_DEF(RawPtr_read_enc_chars_from_i1i2, 4),
	CALLMETHOD_DEF(RawPtr_read_enc_chars_from_subset, 3),
	CALLMETHOD_DEF(RawPtr_write_enc_chars_to_i1i2, 5),
	CALLMETHOD_DEF(RawPtr_write_enc_chars_to_subset, 4),

	CALLMETHOD_DEF(RawPtr_read_complexes_from_i1i2, 4),
	CALLMETHOD_DEF(RawPtr_read_complexes_from_subset, 3),

	CALLMETHOD_DEF(RawPtr_translate_copy_from_i1i2, 5),
	CALLMETHOD_DEF(RawPtr_translate_copy_from_subset, 4),
	CALLMETHOD_DEF(RawPtr_reverse_copy_from_i1i2, 4),
	CALLMETHOD_DEF(RawPtr_reverse_translate_copy_from_i1i2, 5),


/* IntegerPtr_utils.c */
	CALLMETHOD_DEF(debug_IntegerPtr_utils, 0),

	CALLMETHOD_DEF(IntegerPtr_new, 2),
	CALLMETHOD_DEF(IntegerPtr_get_show_string, 1),
	CALLMETHOD_DEF(IntegerPtr_memcmp, 5),

	CALLMETHOD_DEF(IntegerPtr_copy_from_i1i2, 4),
	CALLMETHOD_DEF(IntegerPtr_copy_from_subset, 3),

	CALLMETHOD_DEF(IntegerPtr_read_ints_from_i1i2, 3),
	CALLMETHOD_DEF(IntegerPtr_read_ints_from_subset, 2),
	CALLMETHOD_DEF(IntegerPtr_write_ints_to_i1i2, 4),
	CALLMETHOD_DEF(IntegerPtr_write_ints_to_subset, 3),

/* NumericPtr_utils.c */
	CALLMETHOD_DEF(debug_NumericPtr_utils, 0),

	CALLMETHOD_DEF(NumericPtr_new, 2),
	CALLMETHOD_DEF(NumericPtr_get_show_string, 1),
	CALLMETHOD_DEF(NumericPtr_memcmp, 5),

	CALLMETHOD_DEF(NumericPtr_read_nums_from_i1i2, 3),
	CALLMETHOD_DEF(NumericPtr_read_nums_from_subset, 2),
	CALLMETHOD_DEF(NumericPtr_write_nums_to_i1i2, 4),
	CALLMETHOD_DEF(NumericPtr_write_nums_to_subset, 3),

/* XIntegerViews_class.c */
	CALLMETHOD_DEF(XIntegerViews_slice, 3),

/* XIntegerViews_utils.c */
	CALLMETHOD_DEF(XIntegerViews_viewMins, 2),
	CALLMETHOD_DEF(XIntegerViews_viewMaxs, 2),
	CALLMETHOD_DEF(XIntegerViews_viewSums, 2),
	CALLMETHOD_DEF(XIntegerViews_viewWhichMins, 2),
	CALLMETHOD_DEF(XIntegerViews_viewWhichMaxs, 2),

/* XNumericViews_class.c */
	CALLMETHOD_DEF(XNumericViews_slice, 5),

/* Rle_class.c */
	CALLMETHOD_DEF(Rle_constructor, 2),
	CALLMETHOD_DEF(Rle_run_seqblock, 6),
	CALLMETHOD_DEF(Rle_seqblock, 3),

/* RleViews_utils.c */
	CALLMETHOD_DEF(RleViews_viewMins, 2),
	CALLMETHOD_DEF(RleViews_viewMaxs, 2),
	CALLMETHOD_DEF(RleViews_viewSums, 2),
	CALLMETHOD_DEF(RleViews_viewWhichMins, 2),
	CALLMETHOD_DEF(RleViews_viewWhichMaxs, 2),

/* coverage */
	CALLMETHOD_DEF(IRanges_coverage, 3),

	{NULL, NULL, 0}
};


void R_init_IRanges(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);

/* sort_utils.c */
	REGISTER_CCALLABLE(_sort_int_array);
	REGISTER_CCALLABLE(_get_int_array_order);

/* AEbufs.c */
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
	REGISTER_CCALLABLE(_new_CharAE);
	REGISTER_CCALLABLE(_new_CharAE_from_string);
	REGISTER_CCALLABLE(_CharAE_insert_at);
	REGISTER_CCALLABLE(_append_string_to_CharAE);
	REGISTER_CCALLABLE(_CharAE_asRAW);
	REGISTER_CCALLABLE(_new_CharAEAE);
	REGISTER_CCALLABLE(_CharAEAE_insert_at);
	REGISTER_CCALLABLE(_append_string_to_CharAEAE);
	REGISTER_CCALLABLE(_CharAEAE_asCHARACTER);

/* memcpy_utils.c */
	REGISTER_CCALLABLE(_IRanges_memcmp);
	REGISTER_CCALLABLE(_IRanges_memcpy_from_i1i2);
	REGISTER_CCALLABLE(_IRanges_memcpy_from_subset);
	REGISTER_CCALLABLE(_IRanges_memcpy_to_i1i2);
	REGISTER_CCALLABLE(_IRanges_memcpy_to_subset);
	REGISTER_CCALLABLE(_IRanges_charcpy_from_i1i2_with_lkup);
	REGISTER_CCALLABLE(_IRanges_charcpy_from_subset_with_lkup);
	REGISTER_CCALLABLE(_IRanges_charcpy_to_i1i2_with_lkup);
	REGISTER_CCALLABLE(_IRanges_charcpy_to_subset_with_lkup);
	REGISTER_CCALLABLE(_IRanges_reverse_memcpy_from_i1i2);
	REGISTER_CCALLABLE(_IRanges_reverse_charcpy_from_i1i2_with_lkup);
	REGISTER_CCALLABLE(_IRanges_memcpy_from_i1i2_to_complex);

/* SEXP_utils.c */
	REGISTER_CCALLABLE(_get_classname);

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

/* CompressedIRangesList_class.c */
	REGISTER_CCALLABLE(_cache_CompressedIRangesList);
	REGISTER_CCALLABLE(_get_cachedCompressedIRangesList_elt);

/* Grouping_class.c */
	REGISTER_CCALLABLE(_get_H2LGrouping_high2low);
	REGISTER_CCALLABLE(_get_H2LGrouping_low2high);

/* SequencePtr_class.c */
	REGISTER_CCALLABLE(_new_SequencePtr);
	REGISTER_CCALLABLE(_get_SequencePtr_tag);
	REGISTER_CCALLABLE(_get_SequencePtr_length);

/* XSequence_class.c */
	REGISTER_CCALLABLE(_get_XSequence_xdata);
	REGISTER_CCALLABLE(_get_XSequence_offset);
	REGISTER_CCALLABLE(_get_XSequence_length);
	REGISTER_CCALLABLE(_get_XSequence_tag);
	REGISTER_CCALLABLE(_cache_XRaw);
	REGISTER_CCALLABLE(_new_XSequence);
	REGISTER_CCALLABLE(_new_XRaw_from_tag);
	REGISTER_CCALLABLE(_new_XInteger_from_tag);
	REGISTER_CCALLABLE(_new_XNumeric_from_tag);

	return;
}

