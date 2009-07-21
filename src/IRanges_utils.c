/****************************************************************************
 *                          Fast IRanges utilities                          *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_IRanges_utils()
{
#ifdef DEBUG_IRANGES
	debug = !debug;
	Rprintf("Debug mode turned %s in file %s\n",
		debug ? "on" : "off", __FILE__);
#else
	Rprintf("Debug mode not available in file %s\n", __FILE__);
#endif
	return R_NilValue;
}


/****************************************************************************
 * Reduction (aka extracting the frame)
 */

static RangeAE reduced_ranges;
static int max_end, inframe_offset;

static void add_to_reduced_ranges(int start, int width)
{
	int buf_length, end, gap;

	buf_length = reduced_ranges.start.nelt;
	end = start + width - 1;
	if (buf_length == 0 || (gap = start - max_end - 1) > 0) {
		_RangeAE_insert_at(&reduced_ranges, buf_length, start, width);
		if (buf_length == 0)
			inframe_offset = start - 1;
		else
			inframe_offset += gap;
		max_end = end;
		return;
	}
	if (end <= max_end)
		return;
	reduced_ranges.width.elts[buf_length - 1] += end - max_end;
	max_end = end;
	return;
}

static void reduce_ranges(int length, const int *start, const int *width, int *inframe_start)
{
	int i, j;
	IntAE start_order;

	start_order = _new_IntAE(length, 0, 0);
	_get_int_array_order(start, length, start_order.elts);
	reduced_ranges = _new_RangeAE(0, 0);
	for (i = 0; i < length; i++) {
		j = start_order.elts[i];
		add_to_reduced_ranges(start[j], width[j]);
		if (inframe_start != NULL)
			inframe_start[j] = start[j] - inframe_offset;
	}
	return;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP IRanges_reduce(SEXP x, SEXP with_inframe_start)
{
	int x_length, *inframe_start;
	SEXP x_start, x_width, ans, ans_names, ans_inframe_start;

	x_length = _get_IRanges_length(x);
	x_start = _get_IRanges_start(x);
	x_width = _get_IRanges_width(x);
	if (LOGICAL(with_inframe_start)[0]) {
		PROTECT(ans_inframe_start = NEW_INTEGER(x_length));
		inframe_start = INTEGER(ans_inframe_start);
	} else {
		inframe_start = NULL;
	}
	reduce_ranges(x_length, INTEGER(x_start), INTEGER(x_width), inframe_start);

	PROTECT(ans = NEW_LIST(3));
	PROTECT(ans_names = NEW_CHARACTER(3));
	SET_STRING_ELT(ans_names, 0, mkChar("start"));
	SET_STRING_ELT(ans_names, 1, mkChar("width"));
	SET_STRING_ELT(ans_names, 2, mkChar("inframe.start"));
	SET_NAMES(ans, ans_names);
	UNPROTECT(1);
	SET_ELEMENT(ans, 0, _IntAE_asINTEGER(&(reduced_ranges.start)));
	SET_ELEMENT(ans, 1, _IntAE_asINTEGER(&(reduced_ranges.width)));
	if (inframe_start != NULL) {
		SET_ELEMENT(ans, 2, ans_inframe_start);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */

/* Worst case complexity of O(n^2) :(, but in practice very fast */
SEXP Ranges_disjointBins(SEXP r_start, SEXP r_width)
{
  SEXP ans;
  IntAE bin_ends = _new_IntAE(128, 0, 0);

  PROTECT(ans = NEW_INTEGER(length(r_start)));

  for (int i = 0; i < length(r_start); i++) {
    // find a bin, starting at first
    int j = 0, end = INTEGER(r_start)[i] + INTEGER(r_width)[i] - 1;
    for (; j < bin_ends.nelt && bin_ends.elts[j] >= INTEGER(r_start)[i]; j++);
    // remember when this bin will be open
    if (j == bin_ends.nelt)
      _IntAE_append(&bin_ends, &end, 1);
    else bin_ends.elts[j] = end;
    // store the bin for this range
    INTEGER(ans)[i] = j + 1;
  }

  UNPROTECT(1);
  return ans;
}
