#include "IRanges_interface.h"

#define DEFINE_CCALLABLE_STUB(retT, stubname, Targs, args) \
retT stubname Targs \
{ \
	static retT (*fun)Targs = NULL; \
	if (fun == NULL) \
		fun = (retT (*)Targs) R_GetCCallable("IRanges", "_" #stubname); \
	return fun args; \
}


/*
 * Stubs for callables defined in sort_utils.c
 */

DEFINE_CCALLABLE_STUB(void, sort_int_array,
	(int *x, int x_nelt),
	(     x,     x_nelt)
);

DEFINE_CCALLABLE_STUB(void, get_int_array_order,
	(const int *x, int x_nelt, int *order),
	(           x,     x_nelt,      order)
);


/*
 * Stubs for callables defined in AEbufs.c
 */

DEFINE_CCALLABLE_STUB(void, IntAE_set_val,
	(const IntAE *int_ae, int val),
	(             int_ae,     val)
);

DEFINE_CCALLABLE_STUB(IntAE, new_IntAE,
	(int buflength, int nelt, int val),
	(    buflength,     nelt,     val)
);

DEFINE_CCALLABLE_STUB(void, IntAE_insert_at,
	(IntAE *int_ae, int at, int val),
	(       int_ae,     at,     val)
);

DEFINE_CCALLABLE_STUB(void, IntAE_append,
	(IntAE *int_ae, const int *newvals, int nnewval),
	(       int_ae,            newvals,     nnewval)
);

DEFINE_CCALLABLE_STUB(void, IntAE_delete_at,
	(IntAE *int_ae, int at),
	(       int_ae,     at)
);

DEFINE_CCALLABLE_STUB(void, IntAE_sum_val,
	(const IntAE *int_ae, int val),
	(             int_ae,     val)
);

DEFINE_CCALLABLE_STUB(void, IntAE_append_shifted_vals,
	(IntAE *int_ae, const int *newvals, int nnewval, int shift),
	(       int_ae,            newvals,     nnewval,     shift)
);

DEFINE_CCALLABLE_STUB(void, IntAE_sum_IntAE,
	(const IntAE *int_ae1, const IntAE *int_ae2),
	(             int_ae1,              int_ae2)
);

DEFINE_CCALLABLE_STUB(void, IntAE_qsort,
	(IntAE *int_ae),
	(       int_ae)
);

DEFINE_CCALLABLE_STUB(void, IntAE_delete_adjdups,
	(IntAE *int_ae),
	(       int_ae)
);

DEFINE_CCALLABLE_STUB(SEXP, IntAE_asINTEGER,
	(const IntAE *int_ae),
	(             int_ae)
);

DEFINE_CCALLABLE_STUB(IntAE, INTEGER_asIntAE,
	(SEXP x),
	(     x)
);

DEFINE_CCALLABLE_STUB(IntAE, CHARACTER_asIntAE,
	(SEXP x, int keyshift),
	(     x,     keyshift)
);

DEFINE_CCALLABLE_STUB(IntAEAE, new_IntAEAE,
	(int buflength, int nelt),
	(    buflength,     nelt)
);

DEFINE_CCALLABLE_STUB(void, IntAEAE_insert_at,
	(IntAEAE *int_aeae, int at, const IntAE *int_ae),
	(         int_aeae,     at,              int_ae)
);

DEFINE_CCALLABLE_STUB(void, IntAEAE_eltwise_append,
	(const IntAEAE *int_aeae1, const IntAEAE *int_aeae2),
	(               int_aeae1,                int_aeae2)
);

DEFINE_CCALLABLE_STUB(void, IntAEAE_sum_val,
	(const IntAEAE *int_aeae, int val),
	(               int_aeae,     val)
);

DEFINE_CCALLABLE_STUB(SEXP, IntAEAE_asLIST,
	(const IntAEAE *int_aeae, int mode),
	(               int_aeae,     mode)
);

DEFINE_CCALLABLE_STUB(IntAEAE, LIST_asIntAEAE,
	(SEXP x),
	(     x)
);

DEFINE_CCALLABLE_STUB(SEXP, IntAEAE_toEnvir,
	(const IntAEAE *int_aeae, SEXP envir, int keyshift),
	(               int_aeae,      envir,     keyshift)
);

DEFINE_CCALLABLE_STUB(RangeAE, new_RangeAE,
	(int buflength, int nelt),
	(    buflength,     nelt)
);

DEFINE_CCALLABLE_STUB(void, RangeAE_insert_at,
	(RangeAE *range_ae, int at, int start, int width),
	(         range_ae,     at,     start,     width)
);

DEFINE_CCALLABLE_STUB(CharAE, new_CharAE,
	(int buflength),
	(    buflength)
);

DEFINE_CCALLABLE_STUB(CharAE, new_CharAE_from_string,
	(const char *string),
	(            string)
);

DEFINE_CCALLABLE_STUB(void, CharAE_insert_at,
	(CharAE *char_ae, int at, char c),
	(        char_ae,     at,      c)
);

DEFINE_CCALLABLE_STUB(SEXP, CharAE_asRAW,
	(const CharAE *char_ae),
	(              char_ae)
);

DEFINE_CCALLABLE_STUB(CharAEAE, new_CharAEAE,
	(int buflength, int nelt),
	(    buflength,     nelt)
);

DEFINE_CCALLABLE_STUB(void, CharAEAE_insert_at,
	(CharAEAE *char_aeae, int at, const CharAE *char_ae),
	(          char_aeae,     at,               char_ae)
);

DEFINE_CCALLABLE_STUB(void, append_string_to_CharAEAE,
	(CharAEAE *char_aeae, const char *string),
	(          char_aeae,             string)
);


/*
 * Stubs for callables defined in IRanges_class.c
 */

DEFINE_CCALLABLE_STUB(SEXP, get_IRanges_start,
	(SEXP x),
	(     x)
);

DEFINE_CCALLABLE_STUB(SEXP, get_IRanges_width,
	(SEXP x),
	(     x)
);

DEFINE_CCALLABLE_STUB(int, get_IRanges_length,
	(SEXP x),
	(     x)
);

DEFINE_CCALLABLE_STUB(void, set_IRanges_names,
	(SEXP x, SEXP names),
	(     x,      names)
);

DEFINE_CCALLABLE_STUB(void, copy_IRanges_slots,
	(SEXP x, SEXP x0),
	(     x,      x0)
);

DEFINE_CCALLABLE_STUB(SEXP, new_IRanges,
	(const char *class, SEXP start, SEXP width, SEXP names),
	(            class,      start,      width,      names)
);

DEFINE_CCALLABLE_STUB(SEXP, alloc_IRanges,
	(const char *class, int length),
	(            class,     length)
);

