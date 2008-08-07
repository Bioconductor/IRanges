#include "IRanges.h"

static const R_CallMethodDef callMethods[] = {

/* bufutils.c */
	{"debug_bufutils", (DL_FUNC) &debug_bufutils, 0},

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
	R_RegisterCCallable("IRanges", "_new_CharBuf_from_string", (DL_FUNC) &_new_CharBuf_from_string);
	R_RegisterCCallable("IRanges", "_new_CharBBuf", (DL_FUNC) &_new_CharBBuf);
	R_RegisterCCallable("IRanges", "_append_string_to_CharBBuf", (DL_FUNC) &_append_string_to_CharBBuf);
	R_RegisterCCallable("IRanges", "_get_IRanges_start", (DL_FUNC) &_get_IRanges_start);
	R_RegisterCCallable("IRanges", "_get_IRanges_width", (DL_FUNC) &_get_IRanges_width);
	R_RegisterCCallable("IRanges", "_get_IRanges_length", (DL_FUNC) &_get_IRanges_length);
	R_RegisterCCallable("IRanges", "_set_IRanges_names", (DL_FUNC) &_set_IRanges_names);
	R_RegisterCCallable("IRanges", "_copy_IRanges_slots", (DL_FUNC) &_copy_IRanges_slots);
	R_RegisterCCallable("IRanges", "_new_IRanges_from_RoSeqs", (DL_FUNC) &_new_IRanges_from_RoSeqs);
	R_RegisterCCallable("IRanges", "_alloc_IRanges", (DL_FUNC) &_alloc_IRanges);
}

