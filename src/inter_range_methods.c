/****************************************************************************
 *                         Fast inter-range methods                         *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"

#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)


/****************************************************************************
 * Low-level helper functions.
 */

static int get_maxNROWS_from_CompressedIRangesList_holder(
		const CompressedIRangesList_holder *x_holder)
{
	int x_len, ir_len_max, i, ir_len;

	x_len = _get_length_from_CompressedIRangesList_holder(x_holder);
	ir_len_max = 0;
	for (i = 0; i < x_len; i++) {
		ir_len = _get_eltNROWS_from_CompressedIRangesList_holder(
				x_holder, i);
		if (ir_len > ir_len_max)
			ir_len_max = ir_len;
	}
	return ir_len_max;
}

static int append_IRanges_holder_to_IntPairAE(IntPairAE *intpair_ae,
		const IRanges_holder *ir_holder)
{
	int ir_len, j, start, width;

	ir_len = _get_length_from_IRanges_holder(ir_holder);
	for (j = 0; j < ir_len; j++) {
		start = _get_start_elt_from_IRanges_holder(ir_holder, j);
		width = _get_width_elt_from_IRanges_holder(ir_holder, j);
		IntPairAE_insert_at(intpair_ae, IntPairAE_get_nelt(intpair_ae),
				    start, width);
	}
	return ir_len;
}


/****************************************************************************
 * "range" method.
 */

/* --- .Call ENTRY POINT --- */
SEXP IRanges_range(SEXP x)
{
	int x_len, min, max, i, end;
	const int *start_p, *width_p;
	SEXP ans, ans_start, ans_width;

	x_len = _get_IRanges_length(x);
	if (x_len == 0) {
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
	for (i = 1; i < x_len; i++, start_p++, width_p++)
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
 * "ranges" methods.
 */

/* WARNING: The reduced ranges are *appended* to 'out_ranges'!
   Returns the number of ranges that were appended. */
static int reduce_ranges(const int *x_start, const int *x_width, int x_len,
		int drop_empty_ranges, int min_gapwidth,
		int *order_buf, IntPairAE *out_ranges,
		IntAEAE *revmap, int *out_inframe_start)
{
	int out_len, out_len0, i, j, start_j, width_j, end_j,
	    append_or_drop, max_end, gapwidth, delta, width_inc;
	IntAE *tmp, *revmap_elt;

	if (min_gapwidth < 0)
		error("IRanges internal error in reduce_ranges(): "
		      "negative min_gapwidth not supported");
	get_order_of_int_pairs(x_start, x_width, x_len, 0, 0, order_buf, 0);
	out_len = out_len0 = IntPairAE_get_nelt(out_ranges);
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
			         || start_j != out_ranges->a->elts[
					out_len - 1]))) {
				/* Append to 'out_ranges'. */
				IntPairAE_insert_at(out_ranges,
					out_len,
					start_j, width_j);
				if (revmap != NULL) {
					/* Append to 'revmap'. */
					tmp = new_IntAE(1, 1, j + 1);
					IntAEAE_insert_at(revmap,
							  out_len,
							  tmp);
					revmap_elt = revmap->elts[out_len];
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
				out_ranges->b->elts[out_len - 1] += width_inc;
				max_end = end_j;
			}
			if (!(width_j == 0 && drop_empty_ranges)
			 && revmap != NULL) {
				/* Append to 'revmap'. */
				IntAE_insert_at(revmap_elt,
						IntAE_get_nelt(revmap_elt),
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
		SEXP min_gapwidth, SEXP with_revmap, SEXP with_inframe_start)
{
	int x_len, *inframe_start;
	const int *x_start_p, *x_width_p;
	SEXP ans, ans_names, ans_revmap, ans_inframe_start;
	IntPairAE *out_ranges;
	IntAE *order_buf;
	IntAEAE *revmap;

	x_len = check_integer_pairs(x_start, x_width,
				    &x_start_p, &x_width_p,
				    "start(x)", "width(x)");
	if (LOGICAL(with_revmap)[0]) {
		revmap = new_IntAEAE(0, 0);
	} else {
		revmap = NULL;
	}
	if (LOGICAL(with_inframe_start)[0]) {
		PROTECT(ans_inframe_start = NEW_INTEGER(x_len));
		inframe_start = INTEGER(ans_inframe_start);
	} else {
		inframe_start = NULL;
	}
	out_ranges = new_IntPairAE(0, 0);
	order_buf = new_IntAE(x_len, 0, 0);
	reduce_ranges(x_start_p, x_width_p, x_len,
		LOGICAL(drop_empty_ranges)[0], INTEGER(min_gapwidth)[0],
		order_buf->elts, out_ranges, revmap, inframe_start);

	/* Make 'ans' */
	PROTECT(ans = NEW_LIST(4));
	PROTECT(ans_names = NEW_CHARACTER(4));
	SET_STRING_ELT(ans_names, 0, mkChar("start"));
	SET_STRING_ELT(ans_names, 1, mkChar("width"));
	SET_STRING_ELT(ans_names, 2, mkChar("revmap"));
	SET_STRING_ELT(ans_names, 3, mkChar("inframe.start"));
	SET_NAMES(ans, ans_names);
	UNPROTECT(1);
	SET_VECTOR_ELT(ans, 0, new_INTEGER_from_IntAE(out_ranges->a));
	SET_VECTOR_ELT(ans, 1, new_INTEGER_from_IntAE(out_ranges->b));
	if (revmap != NULL) {
		PROTECT(ans_revmap = new_LIST_from_IntAEAE(revmap, 0));
		SET_VECTOR_ELT(ans, 2, ans_revmap);
		UNPROTECT(1);
	}
	if (inframe_start != NULL) {
		SET_VECTOR_ELT(ans, 3, ans_inframe_start);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP CompressedIRangesList_reduce(SEXP x, SEXP drop_empty_ranges,
		SEXP min_gapwidth, SEXP with_revmap)
{
	SEXP ans, ans_names, ans_revmap, ans_breakpoints;
	     //ans_unlistData, ans_partitioning;
	CompressedIRangesList_holder x_holder;
	IRanges_holder ir_holder;
	int x_len, in_len_max, i;
	IntAE *order_buf;
	IntPairAE *in_ranges, *out_ranges;
	IntAEAE *revmap;

	x_holder = _hold_CompressedIRangesList(x);
	x_len = _get_length_from_CompressedIRangesList_holder(&x_holder);
	if (LOGICAL(with_revmap)[0]) {
		revmap = new_IntAEAE(0, 0);
	} else {
		revmap = NULL;
	}
	in_len_max = get_maxNROWS_from_CompressedIRangesList_holder(&x_holder);
	order_buf = new_IntAE(in_len_max, 0, 0);
	in_ranges = new_IntPairAE(0, 0);
	out_ranges = new_IntPairAE(0, 0);
	PROTECT(ans_breakpoints = NEW_INTEGER(x_len));
	for (i = 0; i < x_len; i++) {
		ir_holder =
		    _get_elt_from_CompressedIRangesList_holder(&x_holder, i);
		IntPairAE_set_nelt(in_ranges, 0);
		append_IRanges_holder_to_IntPairAE(in_ranges, &ir_holder);
		reduce_ranges(in_ranges->a->elts, in_ranges->b->elts,
			IntPairAE_get_nelt(in_ranges),
			LOGICAL(drop_empty_ranges)[0], INTEGER(min_gapwidth)[0],
			order_buf->elts, out_ranges, revmap, NULL);
		INTEGER(ans_breakpoints)[i] = IntPairAE_get_nelt(out_ranges);
	}

	/* Make 'ans' */
	PROTECT(ans = NEW_LIST(4));
	PROTECT(ans_names = NEW_CHARACTER(4));
	SET_STRING_ELT(ans_names, 0, mkChar("start"));
	SET_STRING_ELT(ans_names, 1, mkChar("width"));
	SET_STRING_ELT(ans_names, 2, mkChar("revmap"));
	SET_STRING_ELT(ans_names, 3, mkChar("breakpoints"));
	SET_NAMES(ans, ans_names);
	UNPROTECT(1);
	SET_VECTOR_ELT(ans, 0, new_INTEGER_from_IntAE(out_ranges->a));
	SET_VECTOR_ELT(ans, 1, new_INTEGER_from_IntAE(out_ranges->b));
	if (revmap != NULL) {
		PROTECT(ans_revmap = new_LIST_from_IntAEAE(revmap, 0));
		SET_VECTOR_ELT(ans, 2, ans_revmap);
		UNPROTECT(1);
	}
	SET_VECTOR_ELT(ans, 3, ans_breakpoints);
	UNPROTECT(2);
/*
	PROTECT(ans_unlistData = _new_IRanges_from_IntPairAE("IRanges",
			out_ranges));
	PROTECT(ans_names = duplicate(_get_CompressedList_names(x)));
	PROTECT(ans_partitioning = _new_PartitioningByEnd(
			"PartitioningByEnd", ans_breakpoints, ans_names));
	PROTECT(ans = _new_CompressedList(get_classname(x),
			ans_unlistData, ans_partitioning));
	UNPROTECT(5);
*/
	return ans;
}


/****************************************************************************
 * "gaps" methods.
 */

/* WARNING: The ranges representing the gaps are *appended* to 'out_ranges'!
   Returns the number of ranges that were appended. */
static int gaps_ranges(const int *x_start, const int *x_width, int x_len,
		int restrict_start, int restrict_end,
		int *order_buf, IntPairAE *out_ranges)
{
	int out_len, out_len0, i, j, start_j, width_j, end_j,
	    max_end, gapstart, gapwidth;

	if (restrict_start != NA_INTEGER)
		max_end = restrict_start - 1;
	else
		max_end = NA_INTEGER;
	get_order_of_int_pairs(x_start, x_width, x_len, 0, 0, order_buf, 0);
	out_len = out_len0 = IntPairAE_get_nelt(out_ranges);
	for (i = 0; i < x_len; i++) {
		j = order_buf[i];
		width_j = x_width[j];
		if (width_j == 0)
			continue;
		start_j = x_start[j];
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
				IntPairAE_insert_at(out_ranges,
					out_len,
					gapstart, gapwidth);
				out_len++;
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
		IntPairAE_insert_at(out_ranges,
			out_len,
			gapstart, gapwidth);
		out_len++;
	}
	return out_len - out_len0;
}

/* --- .Call ENTRY POINT --- */
SEXP IRanges_gaps(SEXP x_start, SEXP x_width, SEXP start, SEXP end)
{
	int x_len;
	const int *x_start_p, *x_width_p;
	SEXP ans, ans_names;
	IntPairAE *out_ranges;
	IntAE *order_buf;

	x_len = check_integer_pairs(x_start, x_width,
				    &x_start_p, &x_width_p,
				    "start(x)", "width(x)");
	out_ranges = new_IntPairAE(0, 0);
	order_buf = new_IntAE(x_len, 0, 0);
	gaps_ranges(x_start_p, x_width_p, x_len,
		INTEGER(start)[0], INTEGER(end)[0],
		order_buf->elts, out_ranges);

	PROTECT(ans = NEW_LIST(2));
	PROTECT(ans_names = NEW_CHARACTER(2));
	SET_STRING_ELT(ans_names, 0, mkChar("start"));
	SET_STRING_ELT(ans_names, 1, mkChar("width"));
	SET_NAMES(ans, ans_names);
	UNPROTECT(1);
	SET_VECTOR_ELT(ans, 0, new_INTEGER_from_IntAE(out_ranges->a));
	SET_VECTOR_ELT(ans, 1, new_INTEGER_from_IntAE(out_ranges->b));
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP CompressedIRangesList_gaps(SEXP x, SEXP start, SEXP end)
{
	SEXP ans, ans_names, ans_unlistData, ans_breakpoints, ans_partitioning;
	CompressedIRangesList_holder x_holder;
	IRanges_holder ir_holder;
	int x_len, in_len_max, start_len, end_len, *start_elt, *end_elt, i;
	IntAE *order_buf;
	IntPairAE *in_ranges, *out_ranges;

	x_holder = _hold_CompressedIRangesList(x);
	x_len = _get_length_from_CompressedIRangesList_holder(&x_holder);
	in_len_max = get_maxNROWS_from_CompressedIRangesList_holder(&x_holder);
	order_buf = new_IntAE(in_len_max, 0, 0);
	in_ranges = new_IntPairAE(0, 0);
	out_ranges = new_IntPairAE(0, 0);
	start_len = LENGTH(start);
	end_len = LENGTH(end);
	if (start_len != 1 && start_len != x_len)
		error("'start' must have length 1 or the length of 'x'");
	if (end_len != 1 && end_len != x_len)
		error("'end' must have length 1 or the length of 'x'");
	PROTECT(ans_breakpoints = NEW_INTEGER(x_len));
	start_elt = INTEGER(start);
	end_elt = INTEGER(end);
	for (i = 0; i < x_len; i++) {
		ir_holder =
		    _get_elt_from_CompressedIRangesList_holder(&x_holder, i);
		IntPairAE_set_nelt(in_ranges, 0);
		append_IRanges_holder_to_IntPairAE(in_ranges, &ir_holder);
		gaps_ranges(in_ranges->a->elts, in_ranges->b->elts,
			IntPairAE_get_nelt(in_ranges),
			*start_elt, *end_elt,
			order_buf->elts, out_ranges);
		INTEGER(ans_breakpoints)[i] = IntPairAE_get_nelt(out_ranges);
		if (start_len != 1)
			start_elt++;
		if (end_len != 1)
			end_elt++;
	}
	PROTECT(ans_unlistData = _new_IRanges_from_IntPairAE("IRanges",
			out_ranges));
	PROTECT(ans_names = duplicate(_get_CompressedList_names(x)));
	PROTECT(ans_partitioning = _new_PartitioningByEnd(
			"PartitioningByEnd", ans_breakpoints, ans_names));
	PROTECT(ans = _new_CompressedList(get_classname(x),
			ans_unlistData, ans_partitioning));
	UNPROTECT(5);
	return ans;
}


/****************************************************************************
 * "disjointBins" method.
 */

/* --- .Call ENTRY POINT ---
 * Worst case complexity of O(n^2) :(, but in practice very fast.
 */
SEXP Ranges_disjointBins(SEXP x_start, SEXP x_width)
{
  SEXP ans;
  IntAE *bin_ends = new_IntAE(128, 0, 0);

  PROTECT(ans = NEW_INTEGER(length(x_start)));

  for (int i = 0; i < length(x_start); i++) {
    // find a bin, starting at first
    int j = 0, end = INTEGER(x_start)[i] + INTEGER(x_width)[i] - 1;
    for (; j < IntAE_get_nelt(bin_ends) && bin_ends->elts[j] >= INTEGER(x_start)[i]; j++);
    // remember when this bin will be open
    if (j == IntAE_get_nelt(bin_ends))
      IntAE_append(bin_ends, &end, 1);
    else bin_ends->elts[j] = end;
    // store the bin for this range
    INTEGER(ans)[i] = j + 1;
  }

  UNPROTECT(1);
  return ans;
}

