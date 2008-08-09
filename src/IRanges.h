#include "../inst/include/IRanges_defines.h"
#include <string.h>

#define DEBUG_IRANGES 1


/* AEbufs.c */

SEXP debug_AEbufs();

void _get_intorder(
	int len,
	const int *in,
	int *out
);

void _IntAE_set_val(
	const IntAE *ibuf,
	int val
);

IntAE _new_IntAE(
	int buflength,
	int nelt,
	int val
);

void _IntAE_insert_at(
	IntAE *ibuf,
	int at,
	int val
);

void _IntAE_append(
	IntAE *ibuf,
	const int *newvals,
	int nnewval
);

void _IntAE_delete_at(
	IntAE *ibuf,
	int at
);

void _IntAE_sum_val(
	const IntAE *ibuf,
	int val
);

void _IntAE_append_shifted_vals(
	IntAE *ibuf,
	const int *newvals,
	int nnewval,
	int shift
);

void _IntAE_sum_IntAE(
	const IntAE *ibuf1,
	const IntAE *ibuf2
);

void _IntAE_qsort(IntAE *ibuf);

void _IntAE_delete_consecutiverepeats(IntAE *ibuf);

SEXP _IntAE_asINTEGER(const IntAE *ibuf);

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
	IntAEAE *ibbuf,
	int at,
	const IntAE *ibuf
);

void _IntAEAE_eltwise_append(
	const IntAEAE *ibbuf1,
	const IntAEAE *ibbuf2
);

void _IntAEAE_sum_val(
	const IntAEAE *ibbuf,
	int val
);

SEXP _IntAEAE_asLIST(
	const IntAEAE *ibbuf,
	int mode
);

IntAEAE _LIST_asIntAEAE(SEXP x);

SEXP _IntAEAE_toEnvir(
	const IntAEAE *ibbuf,
	SEXP envir,
	int keyshift
);

RangeAE _new_RangeAE(
	int buflength,
	int nelt
);

void _RangeAE_insert_at(
	RangeAE *rangebuf,
	int at,
	int start,
	int width
);

CharAE _new_CharAE(int buflength);

CharAE _new_CharAE_from_string(const char *string);

void _CharAE_insert_at(
	CharAE *cbuf,
	int at,
	char c
);

SEXP _CharAE_asRAW(const CharAE *cbuf);

CharAEAE _new_CharAEAE(
	int buflength,
	int nelt
);

void _CharAEAE_insert_at(
	CharAEAE *cbbuf,
	int at,
	const CharAE *cbuf
);

void _append_string_to_CharAEAE(
	CharAEAE *cbbuf,
	const char *string
);


/* IRanges_class.c */

SEXP debug_IRanges_class();

SEXP _get_IRanges_start(SEXP x);

SEXP _get_IRanges_width(SEXP x);

int _get_IRanges_length(SEXP x);

const int *_get_IRanges_start0(SEXP x);

const int *_get_IRanges_width0(SEXP x);

void _set_IRanges_names(
	SEXP x,
	SEXP names
);

void _copy_IRanges_slots(
	SEXP x,
	SEXP x0
);

SEXP _new_IRanges(
	const char *class,
	SEXP start,
	SEXP width,
	SEXP names
);

SEXP _alloc_IRanges(
	const char *class,
	int length
);


/* IRanges_utils.c */

SEXP debug_IRanges_utils();

SEXP narrow_IRanges(
	SEXP x,
	SEXP start,
	SEXP end,
	SEXP width
);

SEXP int_to_adjacent_ranges(SEXP x);

SEXP which_as_ranges(SEXP x);

SEXP reduce_IRanges(
	SEXP x,
	SEXP with_inframe_start
);

SEXP IRanges_coverage(
	SEXP x,
	SEXP ans_length,
	SEXP weight
);

SEXP summary_IRanges_list(SEXP x);

