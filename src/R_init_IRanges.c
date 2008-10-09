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
	CALLMETHOD_DEF(sapply_length, 1),
	CALLMETHOD_DEF(safe_strexplode, 1),

/* IRanges_class.c */
	CALLMETHOD_DEF(debug_IRanges_class, 0),

/* solve_user_SEW.c */
	CALLMETHOD_DEF(solve_user_SEW, 6),

/* IRanges_utils.c */
	CALLMETHOD_DEF(debug_IRanges_utils, 0),

	CALLMETHOD_DEF(which_as_IRanges, 1),
	CALLMETHOD_DEF(IRanges_reduce, 2),

/* RangesList_class.c */
	CALLMETHOD_DEF(summary_IRangesList, 1),

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

/* coverage */
	CALLMETHOD_DEF(IRanges_coverage, 3),

/* XIntegerViews.c */
	CALLMETHOD_DEF(XIntegerViews_slice, 3),

/* XIntegerViews_utils.c */
	CALLMETHOD_DEF(XIntegerViews_viewMins, 2),
	CALLMETHOD_DEF(XIntegerViews_viewMaxs, 2),
	CALLMETHOD_DEF(XIntegerViews_viewSums, 2),

/* XNumericViews.c */
	CALLMETHOD_DEF(XNumericViews_slice, 5),

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
	REGISTER_CCALLABLE(_IntAE_sum_val);
	REGISTER_CCALLABLE(_IntAE_append_shifted_vals);
	REGISTER_CCALLABLE(_IntAE_sum_IntAE);
	REGISTER_CCALLABLE(_IntAE_qsort);
	REGISTER_CCALLABLE(_IntAE_delete_adjdups);
	REGISTER_CCALLABLE(_IntAE_asINTEGER);
	REGISTER_CCALLABLE(_INTEGER_asIntAE);
	REGISTER_CCALLABLE(_CHARACTER_asIntAE);
	REGISTER_CCALLABLE(_new_IntAEAE);
	REGISTER_CCALLABLE(_IntAEAE_insert_at);
	REGISTER_CCALLABLE(_IntAEAE_eltwise_append);
	REGISTER_CCALLABLE(_IntAEAE_sum_val);
	REGISTER_CCALLABLE(_IntAEAE_asLIST);
	REGISTER_CCALLABLE(_LIST_asIntAEAE);
	REGISTER_CCALLABLE(_IntAEAE_toEnvir);
	REGISTER_CCALLABLE(_new_RangeAE);
	REGISTER_CCALLABLE(_RangeAE_insert_at);
	REGISTER_CCALLABLE(_new_CharAE);
	REGISTER_CCALLABLE(_new_CharAE_from_string);
	REGISTER_CCALLABLE(_CharAE_insert_at);
	REGISTER_CCALLABLE(_append_string_to_CharAE);
	REGISTER_CCALLABLE(_CharAE_asRAW);
	REGISTER_CCALLABLE(_new_CharAEAE);
	REGISTER_CCALLABLE(_CharAEAE_insert_at);
	REGISTER_CCALLABLE(_append_string_to_CharAEAE);

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
	REGISTER_CCALLABLE(_get_class);

/* IRanges_class.c */
	REGISTER_CCALLABLE(_get_IRanges_start);
	REGISTER_CCALLABLE(_get_IRanges_width);
	REGISTER_CCALLABLE(_get_IRanges_length);
	REGISTER_CCALLABLE(_set_IRanges_names);
	REGISTER_CCALLABLE(_copy_IRanges_slots);
	REGISTER_CCALLABLE(_new_IRanges);
	REGISTER_CCALLABLE(_alloc_IRanges);

/* SequencePtr_class.c */
	REGISTER_CCALLABLE(_new_SequencePtr);
	REGISTER_CCALLABLE(_get_SequencePtr_tag);
	REGISTER_CCALLABLE(_get_SequencePtr_length);

	return;
}

