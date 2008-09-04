#include "IRanges.h"

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#define REGISTER_CCALLABLE(fun) \
	R_RegisterCCallable("IRanges", #fun, (DL_FUNC) &fun)

static const R_CallMethodDef callMethods[] = {

/* AEbufs.c */
	CALLMETHOD_DEF(debug_AEbufs, 0),

/* mem_utils.c */
	CALLMETHOD_DEF(debug_mem_utils, 0),

/* IRanges_class.c */
	CALLMETHOD_DEF(debug_IRanges_class, 0),

/* IRanges_utils.c */
	CALLMETHOD_DEF(debug_IRanges_utils, 0),

	CALLMETHOD_DEF(narrow_IRanges, 4),
	CALLMETHOD_DEF(int_to_adjacent_ranges, 1),
	CALLMETHOD_DEF(which_as_ranges, 1),
	CALLMETHOD_DEF(reduce_IRanges, 2),
	CALLMETHOD_DEF(summary_IRanges_list, 1),

/* utils.c */
	CALLMETHOD_DEF(sapply_length, 1),

/* X_utils.c */
	CALLMETHOD_DEF(debug_X_utils, 0),
	CALLMETHOD_DEF(IRanges_sexp_address, 1),
	CALLMETHOD_DEF(IRanges_xp_show, 1),
	CALLMETHOD_DEF(IRanges_xp_new, 0),

/* XInteger.c */
	CALLMETHOD_DEF(debug_XInteger, 0),

	CALLMETHOD_DEF(XInteger_alloc, 2),
	CALLMETHOD_DEF(XInteger_alloc_initialize, 2),
	CALLMETHOD_DEF(XInteger_get_show_string, 1),
	CALLMETHOD_DEF(XInteger_length, 1),
	CALLMETHOD_DEF(XInteger_memcmp, 5),

	CALLMETHOD_DEF(XInteger_read_ints_from_i1i2, 3),
	CALLMETHOD_DEF(XInteger_read_ints_from_subset, 2),
	CALLMETHOD_DEF(XInteger_write_ints_to_i1i2, 4),
	CALLMETHOD_DEF(XInteger_write_ints_to_subset, 3),
	CALLMETHOD_DEF(XInteger_coverage, 3),

/* XNumeric.c */
	CALLMETHOD_DEF(debug_XNumeric, 0),

	CALLMETHOD_DEF(XNumeric_alloc, 2),
	CALLMETHOD_DEF(XNumeric_get_show_string, 1),
	CALLMETHOD_DEF(XNumeric_length, 1),
	CALLMETHOD_DEF(XNumeric_memcmp, 5),

	CALLMETHOD_DEF(XNumeric_read_nums_from_i1i2, 3),
	CALLMETHOD_DEF(XNumeric_read_nums_from_subset, 2),
	CALLMETHOD_DEF(XNumeric_write_nums_to_i1i2, 4),
	CALLMETHOD_DEF(XNumeric_write_nums_to_subset, 3),

/* XIntegerViews.c */
	CALLMETHOD_DEF(XIntegerViews_slice, 5),

/* XIntegerViews_utils.c */
	CALLMETHOD_DEF(XIntegerViews_viewMins, 2),
	CALLMETHOD_DEF(XIntegerViews_viewMaxs, 2),
	CALLMETHOD_DEF(XIntegerViews_viewSums, 2),

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
	REGISTER_CCALLABLE(_CharAE_asRAW);
	REGISTER_CCALLABLE(_new_CharAEAE);
	REGISTER_CCALLABLE(_CharAEAE_insert_at);
	REGISTER_CCALLABLE(_append_string_to_CharAEAE);

/* mem_utils.c */
	REGISTER_CCALLABLE(_IRanges_memcmp);
	REGISTER_CCALLABLE(_IRanges_memcpy_from_i1i2);
	REGISTER_CCALLABLE(_IRanges_memcpy_from_subset);
	REGISTER_CCALLABLE(_IRanges_memcpy_to_i1i2);
	REGISTER_CCALLABLE(_IRanges_memcpy_to_subset);
	REGISTER_CCALLABLE(_IRanges_translate_charcpy_from_i1i2);
	REGISTER_CCALLABLE(_IRanges_translate_charcpy_from_subset);
	REGISTER_CCALLABLE(_IRanges_translate_charcpy_to_i1i2);
	REGISTER_CCALLABLE(_IRanges_translate_charcpy_to_subset);
	REGISTER_CCALLABLE(_IRanges_reverse_memcpy_from_i1i2);
	REGISTER_CCALLABLE(_IRanges_reverse_translate_charcpy_from_i1i2);
	REGISTER_CCALLABLE(_IRanges_coerce_to_complex_from_i1i2);

/* IRanges_class.c */
	REGISTER_CCALLABLE(_get_IRanges_start);
	REGISTER_CCALLABLE(_get_IRanges_width);
	REGISTER_CCALLABLE(_get_IRanges_length);
	REGISTER_CCALLABLE(_set_IRanges_names);
	REGISTER_CCALLABLE(_copy_IRanges_slots);
	REGISTER_CCALLABLE(_new_IRanges);
	REGISTER_CCALLABLE(_alloc_IRanges);

	return;
}

