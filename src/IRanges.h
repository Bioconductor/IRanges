#include "../inst/include/IRanges_defines.h"
#include <string.h>

#define DEBUG_IRANGES 1


/* bufutils.c */

SEXP debug_bufutils();

void _get_intorder(
	int len,
	const int *in,
	int *out
);

void _IntBuf_set_val(
	const IntBuf *ibuf,
	int val
);

IntBuf _new_IntBuf(
	int buflength,
	int nelt,
	int val
);

void _IntBuf_insert_at(
	IntBuf *ibuf,
	int at,
	int val
);

void _IntBuf_append(
	IntBuf *ibuf,
	const int *newvals,
	int nnewval
);

void _IntBuf_delete_at(
	IntBuf *ibuf,
	int at
);

void _IntBuf_sum_val(
	const IntBuf *ibuf,
	int val
);

void _IntBuf_append_shifted_vals(
	IntBuf *ibuf,
	const int *newvals,
	int nnewval,
	int shift
);

void _IntBuf_sum_IntBuf(
	const IntBuf *ibuf1,
	const IntBuf *ibuf2
);

void _IntBuf_qsort(IntBuf *ibuf);

void _IntBuf_delete_consecutiverepeats(IntBuf *ibuf);

SEXP _IntBuf_asINTEGER(const IntBuf *ibuf);

IntBuf _INTEGER_asIntBuf(SEXP x);

IntBuf _CHARACTER_asIntBuf(
	SEXP x,
	int keyshift
);

IntBBuf _new_IntBBuf(
	int buflength,
	int nelt
);

void _IntBBuf_insert_at(
	IntBBuf *ibbuf,
	int at,
	const IntBuf *ibuf
);

void _IntBBuf_eltwise_append(
	const IntBBuf *ibbuf1,
	const IntBBuf *ibbuf2
);

void _IntBBuf_sum_val(
	const IntBBuf *ibbuf,
	int val
);

SEXP _IntBBuf_asLIST(
	const IntBBuf *ibbuf,
	int mode
);

IntBBuf _LIST_asIntBBuf(SEXP x);

SEXP _IntBBuf_toEnvir(
	const IntBBuf *ibbuf,
	SEXP envir,
	int keyshift
);

RangeBuf _new_RangeBuf(
	int buflength,
	int nelt
);

void _RangeBuf_insert_at(
	RangeBuf *rangebuf,
	int at,
	int start,
	int width
);

CharBuf _new_CharBuf(int buflength);

CharBuf _new_CharBuf_from_string(const char *string);

void _CharBuf_insert_at(
	CharBuf *cbuf,
	int at,
	char c
);

SEXP _CharBuf_asRAW(const CharBuf *cbuf);

CharBBuf _new_CharBBuf(
	int buflength,
	int nelt
);

void _CharBBuf_insert_at(
	CharBBuf *cbbuf,
	int at,
	const CharBuf *cbuf
);

void _append_string_to_CharBBuf(
	CharBBuf *cbbuf,
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

SEXP _new_IRanges_from_RoSeqs(
	const char *class,
	RoSeqs seqs
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

