/****************************************************************************
 *                         Fast inter-range methods                         *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"
#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)

static int debug = 0;

SEXP debug_inter_range_methods()
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
 * Range (min(start), max(end)) of an IRanges object.
 */

/* --- .Call ENTRY POINT --- */
SEXP IRanges_range(SEXP x)
{
	int x_length, min, max, i, end;
	const int *start_p, *width_p;
	SEXP ans, ans_start, ans_width;

	x_length = _get_IRanges_length(x);
	if (x_length == 0) {
		PROTECT(ans_start = NEW_INTEGER(0));
		PROTECT(ans_width = NEW_INTEGER(0));
		PROTECT(ans = _new_IRanges("IRanges", ans_start, ans_width,
					   R_NilValue));
		UNPROTECT(3);
		return ans;
	}
	start_p = INTEGER(_get_IRanges_start(x));
	width_p = INTEGER(_get_IRanges_width(x));
	min = *(start_p++);
	max = min + *(width_p++) - 1;
	for (i = 1; i < x_length; i++, start_p++, width_p++)
	{
		if (*start_p < min)
			min = *start_p;
		end = *start_p + *width_p - 1;
		if (end > max)
			max = end;
	}
	PROTECT(ans_start = ScalarInteger(min));
	PROTECT(ans_width = ScalarInteger(max - min + 1));
	PROTECT(ans = _new_IRanges("IRanges", ans_start, ans_width,
				   R_NilValue));
	UNPROTECT(3);
	return ans;
}


/****************************************************************************
 * Reduction (aka extracting the frame).
 */

/* WARNING: The reduced ranges are *appended* to 'out_ranges'!
   Returns the number of ranges that were appended. */
int _reduce_ranges(const int *x_start, const int *x_width, int x_len,
		int drop_empty_ranges, int min_gapwidth,
		int *order_buf, RangeAE *out_ranges,
		IntAEAE *mapping, int *out_inframe_start)
{
	int out_len, out_len0, i, j, start_j, width_j, end_j,
	    append_or_drop, max_end, gapwidth, delta, width_inc;
	IntAE tmp, *mapping_elt;

	if (min_gapwidth < 0)
		error("IRanges internal error in _reduce_ranges(): "
		      "negative min_gapwidth not supported");
	_get_order_of_int_pairs(x_start, x_width, x_len, 0, order_buf, 0);
	out_len = out_len0 = _RangeAE_get_nelt(out_ranges);
	for (i = 0; i < x_len; i++) {
		j = order_buf[i];
		start_j = x_start[j];
		width_j = x_width[j];
		end_j = start_j + width_j - 1;
		if (i == 0) {
			/* 'append_or_drop' is a toggle that indicates how
			   the current input range should be added to
			   'out_ranges': 1 for appended (or dropped), 0 for
			   merged. */
			append_or_drop = 1;
			max_end = end_j;
			delta = start_j - 1;
		} else {
			/* If 'i' != 0 and 'append_or_drop' is 1 then the
			   previous range was empty so 'gapwidth' will be
			   >= 0. */
			gapwidth = start_j - max_end - 1;
			if (gapwidth >= min_gapwidth)
				append_or_drop = 1;
		}
		if (append_or_drop) {
			if (width_j != 0
			 || (!drop_empty_ranges
			     && (out_len == out_len0
			         || start_j != out_ranges->start.elts[
					out_len - 1]))) {
				/* Append to 'out_ranges'. */
				_RangeAE_insert_at(out_ranges,
					out_len,
					start_j, width_j);
				if (mapping != NULL) {
					/* Append to 'mapping'. */
					tmp = _new_IntAE(1, 1, j + 1);
					_IntAEAE_insert_at(mapping,
							   out_len,
							   &tmp);
					mapping_elt = mapping->elts + out_len;
				}
				out_len++;
				append_or_drop = 0;
			}
			max_end = end_j;
			if (i != 0)
				delta += gapwidth;
		} else {
			width_inc = end_j - max_end;
			if (width_inc > 0) {
				/* Merge with last range in 'out_ranges'. */
				out_ranges->width.elts[out_len - 1] += width_inc;
				max_end = end_j;
			}
			if (!(width_j == 0 && drop_empty_ranges)
			 && mapping != NULL) {
				_IntAE_insert_at(mapping_elt,
						 _IntAE_get_nelt(mapping_elt),
						 j + 1);
			}
		}
		if (out_inframe_start != NULL) 
			out_inframe_start[j] = start_j - delta;
	}
	return out_len - out_len0;
}

/* --- .Call ENTRY POINT --- */
SEXP Ranges_reduce(SEXP x_start, SEXP x_width, SEXP drop_empty_ranges,
		SEXP min_gapwidth, SEXP with_mapping, SEXP with_inframe_start)
{
	int x_len, *inframe_start;
	const int *x_start_p, *x_width_p;
	SEXP ans, ans_names, ans_mapping, ans_inframe_start;
	RangeAE out_ranges;
	IntAE order_buf;
	IntAEAE tmp, *mapping;

	x_len = _check_integer_pairs(x_start, x_width,
				     &x_start_p, &x_width_p,
				     "start(x)", "width(x)");
	if (LOGICAL(with_mapping)[0]) {
		tmp = _new_IntAEAE(0, 0);
		mapping = &tmp;
	} else {
		mapping = NULL;
	}
	if (LOGICAL(with_inframe_start)[0]) {
		PROTECT(ans_inframe_start = NEW_INTEGER(x_len));
		inframe_start = INTEGER(ans_inframe_start);
	} else {
		inframe_start = NULL;
	}
	out_ranges = _new_RangeAE(0, 0);
	order_buf = _new_IntAE(x_len, 0, 0);
	_reduce_ranges(x_start_p, x_width_p, x_len,
		LOGICAL(drop_empty_ranges)[0], INTEGER(min_gapwidth)[0],
		order_buf.elts, &out_ranges, mapping, inframe_start);

	PROTECT(ans = NEW_LIST(4));
	PROTECT(ans_names = NEW_CHARACTER(4));
	SET_STRING_ELT(ans_names, 0, mkChar("start"));
	SET_STRING_ELT(ans_names, 1, mkChar("width"));
	SET_STRING_ELT(ans_names, 2, mkChar("mapping"));
	SET_STRING_ELT(ans_names, 3, mkChar("inframe.start"));
	SET_NAMES(ans, ans_names);
	UNPROTECT(1);
	SET_VECTOR_ELT(ans, 0, _new_INTEGER_from_IntAE(&(out_ranges.start)));
	SET_VECTOR_ELT(ans, 1, _new_INTEGER_from_IntAE(&(out_ranges.width)));
	if (mapping != NULL) {
		PROTECT(ans_mapping = _new_LIST_from_IntAEAE(mapping, 0));
		SET_VECTOR_ELT(ans, 2, ans_mapping);
		UNPROTECT(1);
	}
	if (inframe_start != NULL) {
		SET_VECTOR_ELT(ans, 3, ans_inframe_start);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Extracting the gaps.
 */

/* WARNING: The ranges representing the gaps are *appended* to 'out_ranges'!
   Returns the number of ranges that were appended. */
int _gaps_ranges(const int *start, const int *width, int length,
		int restrict_start, int restrict_end,
		int *order_buf, RangeAE *out_ranges)
{
	int out_length, i, j, start_j, width_j, end_j,
	    max_end, gapstart, gapwidth;

	if (restrict_start != NA_INTEGER)
		max_end = restrict_start - 1;
	else
		max_end = NA_INTEGER;
	_get_order_of_int_pairs(start, width, length, 0, order_buf, 0);
	out_length = 0;
	for (i = 0; i < length; i++) {
		j = order_buf[i];
		width_j = width[j];
		if (width_j == 0)
			continue;
		start_j = start[j];
		end_j = start_j + width_j - 1;
		if (max_end == NA_INTEGER) {
			max_end = end_j;
		} else {
			gapstart = max_end + 1;
			if (restrict_end != NA_INTEGER
			 && start_j > restrict_end + 1)
				start_j = restrict_end + 1;
			gapwidth = start_j - gapstart;
			if (gapwidth >= 1) {
				/* Append to 'out_ranges'. */
				_RangeAE_insert_at(out_ranges,
					_RangeAE_get_nelt(out_ranges),
					gapstart, gapwidth);
				out_length++;
				max_end = end_j;
			} else if (end_j > max_end) {
				max_end = end_j;
			}
		}
		if (restrict_end != NA_INTEGER && max_end >= restrict_end)
			break;
	}
	if (restrict_end != NA_INTEGER
	 && max_end != NA_INTEGER
	 && max_end < restrict_end) {
		gapstart = max_end + 1;
		gapwidth = restrict_end - max_end;
		/* Append to 'out_ranges'. */
		_RangeAE_insert_at(out_ranges,
			_RangeAE_get_nelt(out_ranges),
			gapstart, gapwidth);
		out_length++;
	}
	return out_length;
}

/* --- .Call ENTRY POINT --- */
SEXP IRanges_gaps(SEXP x, SEXP start, SEXP end)
{
	int x_length;
	SEXP x_start, x_width, ans, ans_names;
	RangeAE out_ranges;
	IntAE order_buf;

	x_length = _get_IRanges_length(x);
	x_start = _get_IRanges_start(x);
	x_width = _get_IRanges_width(x);
	out_ranges = _new_RangeAE(0, 0);
	order_buf = _new_IntAE(x_length, 0, 0);
	_gaps_ranges(INTEGER(x_start), INTEGER(x_width), x_length,
		INTEGER(start)[0], INTEGER(end)[0],
		order_buf.elts, &out_ranges);

	PROTECT(ans = NEW_LIST(2));
	PROTECT(ans_names = NEW_CHARACTER(2));
	SET_STRING_ELT(ans_names, 0, mkChar("start"));
	SET_STRING_ELT(ans_names, 1, mkChar("width"));
	SET_NAMES(ans, ans_names);
	UNPROTECT(1);
	SET_VECTOR_ELT(ans, 0, _new_INTEGER_from_IntAE(&(out_ranges.start)));
	SET_VECTOR_ELT(ans, 1, _new_INTEGER_from_IntAE(&(out_ranges.width)));
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
    for (; j < _IntAE_get_nelt(&bin_ends) && bin_ends.elts[j] >= INTEGER(r_start)[i]; j++);
    // remember when this bin will be open
    if (j == _IntAE_get_nelt(&bin_ends))
      _IntAE_append(&bin_ends, &end, 1);
    else bin_ends.elts[j] = end;
    // store the bin for this range
    INTEGER(ans)[i] = j + 1;
  }

  UNPROTECT(1);
  return ans;
}

