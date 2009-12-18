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
 * Reduction (aka extracting the frame).
 */

/* WARNING: The reduced ranges are *appended* to 'out_ranges'!
   Returns the number of ranges that were appended. */
int _reduce_ranges(const int *start, const int *width, int length,
		int drop_empty_ranges,
		int *tmpbuf, RangeAE *out_ranges, int *out_inframe_start)
{
	int out_length, i, j, start_j, width_j, end_j,
	    gap, max_end, inframe_offset;

	_get_order_of_int_array(start, length, 0, tmpbuf, 0);
	out_length = 0;
	for (i = 0; i < length; i++) {
		j = tmpbuf[i];
		start_j = start[j];
		width_j = width[j];
		end_j = start_j + width_j - 1;
		if (out_length == 0 || (gap = start_j - max_end - 1) > 0) {
			if (width_j != 0 || !drop_empty_ranges)
				_RangeAE_insert_at(out_ranges,
					out_ranges->start.nelt,
					start_j, width_j);
			max_end = end_j;
			if (out_length == 0)
				inframe_offset = start_j - 1;
			else
				inframe_offset += gap;
			out_length++;
		} else if (end_j > max_end) {
			out_ranges->width.elts[out_ranges->width.nelt - 1] +=
					end_j - max_end;
			max_end = end_j;
		}
		if (out_inframe_start != NULL)
			out_inframe_start[j] = start_j - inframe_offset;
	}
	return out_length;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP IRanges_reduce(SEXP x, SEXP drop_empty_ranges, SEXP with_inframe_start)
{
	int x_length, *inframe_start;
	SEXP x_start, x_width, ans, ans_names, ans_inframe_start;
	RangeAE out_ranges;
	IntAE tmpbuf;

	x_length = _get_IRanges_length(x);
	x_start = _get_IRanges_start(x);
	x_width = _get_IRanges_width(x);
	if (LOGICAL(with_inframe_start)[0]) {
		PROTECT(ans_inframe_start = NEW_INTEGER(x_length));
		inframe_start = INTEGER(ans_inframe_start);
	} else {
		inframe_start = NULL;
	}
	out_ranges = _new_RangeAE(0, 0);
	tmpbuf = _new_IntAE(x_length, 0, 0);
	_reduce_ranges(INTEGER(x_start), INTEGER(x_width), x_length,
			LOGICAL(drop_empty_ranges)[0],
			tmpbuf.elts, &out_ranges, inframe_start);

	PROTECT(ans = NEW_LIST(3));
	PROTECT(ans_names = NEW_CHARACTER(3));
	SET_STRING_ELT(ans_names, 0, mkChar("start"));
	SET_STRING_ELT(ans_names, 1, mkChar("width"));
	SET_STRING_ELT(ans_names, 2, mkChar("inframe.start"));
	SET_NAMES(ans, ans_names);
	UNPROTECT(1);
	SET_VECTOR_ELT(ans, 0, _IntAE_asINTEGER(&(out_ranges.start)));
	SET_VECTOR_ELT(ans, 1, _IntAE_asINTEGER(&(out_ranges.width)));
	if (inframe_start != NULL) {
		SET_VECTOR_ELT(ans, 2, ans_inframe_start);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Other Ranges utilities.
 */

/* --- .Call ENTRY POINT ---
 * Worst case complexity of O(n^2) :(, but in practice very fast.
 */
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

