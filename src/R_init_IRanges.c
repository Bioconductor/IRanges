#include "IRanges.h"

static const R_CallMethodDef callMethods[] = {

/* AEbufs.c */
	{"debug_AEbufs", (DL_FUNC) &debug_AEbufs, 0},

/* IRanges_class.c */
	{"debug_IRanges_class", (DL_FUNC) &debug_IRanges_class, 0},

/* IRanges_utils.c */
	{"debug_IRanges_utils", (DL_FUNC) &debug_IRanges_utils, 0},

	{"narrow_IRanges", (DL_FUNC) &narrow_IRanges, 4},
	{"int_to_adjacent_ranges", (DL_FUNC) &int_to_adjacent_ranges, 1},
	{"which_as_ranges", (DL_FUNC) &which_as_ranges, 1},
	{"reduce_IRanges", (DL_FUNC) &reduce_IRanges, 2},
	{"IRanges_coverage", (DL_FUNC) &IRanges_coverage, 3},
	{"summary_IRanges_list", (DL_FUNC) &summary_IRanges_list, 1},

	{NULL, NULL, 0}
};


void R_init_IRanges(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);

/* AEbufs.c */
	R_RegisterCCallable("IRanges", "_IntAE_set_val", (DL_FUNC) &_IntAE_set_val);
	R_RegisterCCallable("IRanges", "_new_IntAE", (DL_FUNC) &_new_IntAE);
	R_RegisterCCallable("IRanges", "_IntAE_insert_at", (DL_FUNC) &_IntAE_insert_at);
	R_RegisterCCallable("IRanges", "_IntAE_append", (DL_FUNC) &_IntAE_append);
	R_RegisterCCallable("IRanges", "_IntAE_delete_at", (DL_FUNC) &_IntAE_delete_at);
	R_RegisterCCallable("IRanges", "_IntAE_sum_val", (DL_FUNC) &_IntAE_sum_val);
	R_RegisterCCallable("IRanges", "_IntAE_append_shifted_vals", (DL_FUNC) &_IntAE_append_shifted_vals);
	R_RegisterCCallable("IRanges", "_IntAE_sum_IntAE", (DL_FUNC) &_IntAE_sum_IntAE);
	R_RegisterCCallable("IRanges", "_IntAE_qsort", (DL_FUNC) &_IntAE_qsort);
	R_RegisterCCallable("IRanges", "_IntAE_delete_adjdups", (DL_FUNC) &_IntAE_delete_adjdups);
	R_RegisterCCallable("IRanges", "_IntAE_asINTEGER", (DL_FUNC) &_IntAE_asINTEGER);
	R_RegisterCCallable("IRanges", "_INTEGER_asIntAE", (DL_FUNC) &_INTEGER_asIntAE);
	R_RegisterCCallable("IRanges", "_CHARACTER_asIntAE", (DL_FUNC) &_CHARACTER_asIntAE);
	R_RegisterCCallable("IRanges", "_new_IntAEAE", (DL_FUNC) &_new_IntAEAE);
	R_RegisterCCallable("IRanges", "_IntAEAE_insert_at", (DL_FUNC) &_IntAEAE_insert_at);
	R_RegisterCCallable("IRanges", "_IntAEAE_eltwise_append", (DL_FUNC) &_IntAEAE_eltwise_append);
	R_RegisterCCallable("IRanges", "_IntAEAE_sum_val", (DL_FUNC) &_IntAEAE_sum_val);
	R_RegisterCCallable("IRanges", "_IntAEAE_asLIST", (DL_FUNC) &_IntAEAE_asLIST);
	R_RegisterCCallable("IRanges", "_LIST_asIntAEAE", (DL_FUNC) &_LIST_asIntAEAE);
	R_RegisterCCallable("IRanges", "_IntAEAE_toEnvir", (DL_FUNC) &_IntAEAE_toEnvir);
	R_RegisterCCallable("IRanges", "_new_RangeAE", (DL_FUNC) &_new_RangeAE);
	R_RegisterCCallable("IRanges", "_RangeAE_insert_at", (DL_FUNC) &_RangeAE_insert_at);
	R_RegisterCCallable("IRanges", "_new_CharAE", (DL_FUNC) &_new_CharAE);
	R_RegisterCCallable("IRanges", "_new_CharAE_from_string", (DL_FUNC) &_new_CharAE_from_string);
	R_RegisterCCallable("IRanges", "_CharAE_insert_at", (DL_FUNC) &_CharAE_insert_at);
	R_RegisterCCallable("IRanges", "_CharAE_asRAW", (DL_FUNC) &_CharAE_asRAW);
	R_RegisterCCallable("IRanges", "_new_CharAEAE", (DL_FUNC) &_new_CharAEAE);
	R_RegisterCCallable("IRanges", "_CharAEAE_insert_at", (DL_FUNC) &_CharAEAE_insert_at);
	R_RegisterCCallable("IRanges", "_append_string_to_CharAEAE", (DL_FUNC) &_append_string_to_CharAEAE);

/* IRanges_class.c */
	R_RegisterCCallable("IRanges", "_get_IRanges_start", (DL_FUNC) &_get_IRanges_start);
	R_RegisterCCallable("IRanges", "_get_IRanges_width", (DL_FUNC) &_get_IRanges_width);
	R_RegisterCCallable("IRanges", "_get_IRanges_length", (DL_FUNC) &_get_IRanges_length);
	R_RegisterCCallable("IRanges", "_set_IRanges_names", (DL_FUNC) &_set_IRanges_names);
	R_RegisterCCallable("IRanges", "_copy_IRanges_slots", (DL_FUNC) &_copy_IRanges_slots);
	R_RegisterCCallable("IRanges", "_new_IRanges", (DL_FUNC) &_new_IRanges);
	R_RegisterCCallable("IRanges", "_alloc_IRanges", (DL_FUNC) &_alloc_IRanges);
}

