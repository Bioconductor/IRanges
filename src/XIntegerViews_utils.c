#include "IRanges.h"

#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XInteger_slice(SEXP x, SEXP lower, SEXP upper)
{
	cachedIntSeq X;
	SEXP ans, start, width;
	int i, ans_length;
	const int *X_elt;
	int *start_elt, *width_elt, lower_elt, upper_elt, curr_elt, prev_elt;

	lower_elt = INTEGER(lower)[0];
	upper_elt = INTEGER(upper)[0];

	X = _cache_XInteger(x);
	ans_length = 0;
	prev_elt = 0;
	for (i = 1, X_elt = X.seq; i <= X.length; i++, X_elt++) {
		curr_elt = (*X_elt >= lower_elt) && (*X_elt <= upper_elt);
		if (curr_elt && !prev_elt)
			ans_length++;
		prev_elt = curr_elt;
	}

	PROTECT(start = NEW_INTEGER(ans_length));
	PROTECT(width = NEW_INTEGER(ans_length));
	if (ans_length > 0) {
		start_elt = INTEGER(start) - 1;
		width_elt = INTEGER(width) - 1;
		prev_elt = 0;
		for (i = 1, X_elt = X.seq; i <= X.length; i++, X_elt++) {
			curr_elt = (*X_elt >= lower_elt) && (*X_elt <= upper_elt);
			if (curr_elt) {
				if (prev_elt)
					*width_elt += 1;
				else {
					start_elt++;
					width_elt++;
					*start_elt = i;
					*width_elt = 1;
				}
			}
			prev_elt = curr_elt;
		}
	}
	PROTECT(ans = _new_IRanges("IRanges", start, width, R_NilValue));
	UNPROTECT(3);
	return ans;
}


/****************************************************************************
 * Low-level operations on cachedIntSeq structures (sequences of ints).
 */

/*
 * Returns NA_INTEGER if 'X' is empty. Note that this differs from what min()
 * does on a standard integer vector (it returns Inf, which is a double).
 * See C function imin() defined in the R source code (src/main/summary.c)
 * for the details.
 */
static int get_cachedIntSeq_min(const cachedIntSeq *X, int narm)
{
	int xlen, val, i, x;

	xlen = X->length;
	val = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->seq[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return NA_INTEGER;
		}
		if (val == NA_INTEGER || x < val)
			val = x;
	}
	return val;
}

/*
 * Returns NA_INTEGER if 'X' is empty. Note that this differs from what max()
 * does on a standard integer vector (it returns -Inf, which is a double).
 * See C function imax() defined in the R source code (src/main/summary.c)
 * for the details.
 */
static int get_cachedIntSeq_max(const cachedIntSeq *X, int narm)
{
	int xlen, val, i, x;

	xlen = X->length;
	val = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->seq[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return NA_INTEGER;
		}
		if (val == NA_INTEGER || x > val)
			val = x;
	}
	return val;
}

static int get_cachedIntSeq_sum(const cachedIntSeq *X, int narm)
{
	int xlen, val, i, x;

	xlen = X->length;
	val = 0;
	for (i = 0; i < xlen; i++) {
		x = X->seq[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return NA_INTEGER;
		}
		if ((x > 0 && INT_MAX - x < val)
		 || (x < 0 && R_INT_MIN - x > val)) {
			warning("Integer overflow");
			return NA_INTEGER;
		}
		val += x;
	}
	return val;
}

static int get_cachedIntSeq_min_offset(const cachedIntSeq *X, int narm)
{
	int xlen, cur_min, offset, i, x;

	xlen = X->length;
	offset = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->seq[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return xlen == 1 ? 0 : NA_INTEGER;
		}
		if (offset == NA_INTEGER || x < cur_min) {
			cur_min = x;
			offset = i;
		}
	}
	return offset;
}

static int get_cachedIntSeq_max_offset(const cachedIntSeq *X, int narm)
{
	int xlen, cur_max, offset, i, x;

	xlen = X->length;
	offset = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->seq[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return xlen == 1 ? 0 : NA_INTEGER;
		}
		if (offset == NA_INTEGER || x > cur_max) {
			cur_max = x;
			offset = i;
		}
	}
	return offset;
}


/****************************************************************************
 * XIntegerViews_view* .Call entry points for fast view summary methods:
 * viewMins, viewMaxs, viewSums, viewWhichMins, viewWhichMaxs.
 *
 * TODO: They don't support "out of limits" views right now. An easy solution
 * for viewMins/viewMaxs/viewSums would be to trim the views in R before 'x'
 * is passed to the .Call entry point. Note that this solution would not work
 * for viewWhichMins/viewWhichMaxs though.
 */

SEXP XIntegerViews_viewMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	int ans_length, v, *ans_elt, view_start, view_width, view_offset;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (v = 0, ans_elt = INTEGER(ans); v < ans_length; v++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, v);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, v);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewMins() doesn't support \"out of limits\" "
			      "views in XIntegerViews objects yet, sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		*ans_elt = get_cachedIntSeq_min(&S_view, LOGICAL(na_rm)[0]);
	}
	UNPROTECT(1);
	return ans;
}

SEXP XIntegerViews_viewMaxs(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	int ans_length, v, *ans_elt, view_start, view_width, view_offset;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (v = 0, ans_elt = INTEGER(ans); v < ans_length; v++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, v);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, v);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewMaxs() doesn't support \"out of limits\" "
			      "views in XIntegerViews objects yet, sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		*ans_elt = get_cachedIntSeq_max(&S_view, LOGICAL(na_rm)[0]);
	}
	UNPROTECT(1);
	return ans;
}

SEXP XIntegerViews_viewSums(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	int ans_length, v, *ans_elt, view_start, view_width, view_offset;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (v = 0, ans_elt = INTEGER(ans); v < ans_length; v++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, v);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, v);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewSums() doesn't support \"out of limits\" "
			      "views in XIntegerViews objects yet, sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		*ans_elt = get_cachedIntSeq_sum(&S_view, LOGICAL(na_rm)[0]);
	}
	UNPROTECT(1);
	return ans;
}

SEXP XIntegerViews_viewWhichMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	int ans_length, v, *ans_elt, view_start, view_width, view_offset,
	    min_offset;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (v = 0, ans_elt = INTEGER(ans); v < ans_length; v++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, v);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, v);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewWhichMins() doesn't support \"out of "
			      "limits\" views in XIntegerViews objects yet, "
			      "sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		min_offset = get_cachedIntSeq_min_offset(&S_view,
						LOGICAL(na_rm)[0]);
		*ans_elt = min_offset == NA_INTEGER ? min_offset
						    : view_start + min_offset;
	}
	UNPROTECT(1);
	return ans;
}

SEXP XIntegerViews_viewWhichMaxs(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	int ans_length, v, *ans_elt, view_start, view_width, view_offset,
	    max_offset;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (v = 0, ans_elt = INTEGER(ans); v < ans_length; v++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, v);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, v);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewWhichMaxs() doesn't support \"out of "
			      "limits\" views in XIntegerViews objects yet, "
			      "sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		max_offset = get_cachedIntSeq_max_offset(&S_view,
						LOGICAL(na_rm)[0]);
		*ans_elt = max_offset == NA_INTEGER ? max_offset
						    : view_start + max_offset;
	}
	UNPROTECT(1);
	return ans;
}

