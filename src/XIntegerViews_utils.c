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

static cachedIntSeq get_cachedIntSeq_view(const cachedIntSeq *X,
		int view_start, int view_width)
{
	cachedIntSeq X_view;
	int view_offset, tmp;

	view_offset = view_start - 1;
	/* Trim the view if it's "out of limits". */
	if (view_offset < 0) {
		view_width += view_offset;
		view_offset = 0;
	}
	if (view_width > (tmp = X->length - view_offset))
		view_width = tmp;
	X_view.seq = X->seq + view_offset;
	X_view.length = view_width;
	return X_view;
}

/*
 * Returns NA if 'X' is empty. Note that this differs from what
 * 'min(integer(0))' does: the latter returns 'Inf' (which is a double) and
 * issues a warning.
 * See C function imin() in the R source code (src/main/summary.c) for the
 * details.
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
 * Returns NA if 'X' is empty. Note that this differs from what
 * 'max(integer(0))' does: the latter returns '-Inf' (which is a double) and
 * issues a warning.
 * See C function imax() in the R source code (src/main/summary.c) for the
 * details.
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

/* TODO: Compare the code below with what which.min() does on a standard
 * integer vector. */
static int get_cachedIntSeq_which_min(const cachedIntSeq *X, int narm)
{
	int xlen, cur_min, which_min, i, x;

	xlen = X->length;
	which_min = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->seq[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return xlen == 1 ? 1 : NA_INTEGER;
		}
		if (which_min == NA_INTEGER || x < cur_min) {
			cur_min = x;
			which_min = i + 1;
		}
	}
	return which_min;
}

/* TODO: Compare the code below with what which.max() does on a standard
 * integer vector. */
static int get_cachedIntSeq_which_max(const cachedIntSeq *X, int narm)
{
	int xlen, cur_max, which_max, i, x;

	xlen = X->length;
	which_max = NA_INTEGER;
	for (i = 0; i < xlen; i++) {
		x = X->seq[i];
		if (x == NA_INTEGER) {
			if (narm)
				continue;
			return xlen == 1 ? 1 : NA_INTEGER;
		}
		if (which_max == NA_INTEGER || x > cur_max) {
			cur_max = x;
			which_max = i + 1;
		}
	}
	return which_max;
}


/****************************************************************************
 * XIntegerViews_summary1() .Call entry points for fast view summary methods:
 * viewMins, viewMaxs, viewSums.
 */

SEXP XIntegerViews_summary1(SEXP x, SEXP na_rm, SEXP method)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	const char *funname;
	int (*fun)(const cachedIntSeq *, int);
	int ans_length, v, view_start, view_width, *ans_elt;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	funname = CHAR(STRING_ELT(method, 0));
	if (strcmp(funname, "viewMins") == 0)
		fun = &get_cachedIntSeq_min;
	else if (strcmp(funname, "viewMaxs") == 0)
		fun = &get_cachedIntSeq_max;
	else if (strcmp(funname, "viewSums") == 0)
		fun = &get_cachedIntSeq_sum;
	else
		error("IRanges internal error in XIntegerViews_summary1(): "
		      "invalid method \"%s\"", funname);
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (v = 0, ans_elt = INTEGER(ans); v < ans_length; v++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, v);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, v);
		S_view = get_cachedIntSeq_view(&S, view_start, view_width);
		*ans_elt = fun(&S_view, LOGICAL(na_rm)[0]);
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * XIntegerViews_summary2() .Call entry points for fast view summary methods:
 * viewWhichMins, viewWhichMaxs.
 */

SEXP XIntegerViews_summary2(SEXP x, SEXP na_rm, SEXP method)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	const char *funname;
	int (*fun)(const cachedIntSeq *, int);
	int ans_length, v, view_start, view_width, *ans_elt, which_min;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	funname = CHAR(STRING_ELT(method, 0));
	if (strcmp(funname, "viewWhichMins") == 0)
		fun = &get_cachedIntSeq_which_min;
	else if (strcmp(funname, "viewWhichMaxs") == 0)
		fun = &get_cachedIntSeq_which_max;
	else
		error("IRanges internal error in XIntegerViews_summary2(): "
		      "invalid method \"%s\"", funname);
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (v = 0, ans_elt = INTEGER(ans); v < ans_length; v++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, v);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, v);
		S_view = get_cachedIntSeq_view(&S, view_start, view_width);
		which_min = fun(&S_view, LOGICAL(na_rm)[0]);
		if (which_min == NA_INTEGER)
			*ans_elt = which_min;
		else
			*ans_elt = S_view.seq - S.seq + which_min;
	}
	UNPROTECT(1);
	return ans;
}

