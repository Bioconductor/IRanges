#include "IRanges.h"

static int gt(double x, double y) {
	return x > y;
}

static int lt(double x, double y) {
	return x < y;
}

static int ge(double x, double y) {
	return x >= y;
}

static int le(double x, double y) {
	return x <= y;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XDouble_slice(SEXP x, SEXP lower, SEXP upper,
		SEXP include_lower, SEXP include_upper)
{
	cachedDoubleSeq X;
	SEXP ans, start, width;
	int i, ans_length;
	const double *X_elt;
	int *start_elt, *width_elt, curr_elt, prev_elt;
	double lower_elt, upper_elt;
	int (*lower_fun)(double, double);
	int (*upper_fun)(double, double);

	lower_fun = LOGICAL(include_lower)[0] ? &ge : &gt;
	upper_fun = LOGICAL(include_upper)[0] ? &le : &lt;

	lower_elt = REAL(lower)[0];
	upper_elt = REAL(upper)[0];

	X = _cache_XDouble(x);
	ans_length = 0;
	prev_elt = 0;
	for (i = 1, X_elt = X.seq; i <= X.length; i++, X_elt++) {
		curr_elt = lower_fun(*X_elt, lower_elt) && upper_fun(*X_elt, upper_elt);
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
			curr_elt = lower_fun(*X_elt, lower_elt) && upper_fun(*X_elt, upper_elt);
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

/* ----------------------------------------------------------------------------
 * These were copied wholesale from XIntegerViews_utils.c and need translation
 */

/*
 * --- .Call ENTRY POINT ---
 * TODO: Support "out of limits" views. An easy solution would be to trim the
 * views in R before 'x' is passed to XDoubleViews_viewMins().
 */
SEXP XDoubleViews_viewMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedDoubleSeq S, S_view;
	cachedIRanges cached_ranges;
	const double *S_view_elt;
	int i, j, ans_length, view_start, view_width, view_offset;
	double *ans_elt;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XDouble(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_NUMERIC(ans_length));
	for (i = 0, ans_elt = REAL(ans); i < ans_length; i++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewMins() doesn't support \"out of limits\" "
			      "views in XDoubleViews objects yet, sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		/* The code below does something *close* but not identical to
		 * what min() does on a standard double vector. See rmin()
		 * C static function in the R source code (src/main/summary.c)
		 * for what standard min() does. In particular, how they handle
		 * NA/NaN values seems slightly different. */
		*ans_elt = R_PosInf;
		for (j = 0, S_view_elt = S_view.seq;
		     j < S_view.length;
		     j++, S_view_elt++)
		{
			if (ISNAN(*S_view_elt)) { /* NA or NaN */
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = *S_view_elt;
					break;
				}
			} else if (*S_view_elt < *ans_elt)
				*ans_elt = *S_view_elt;
		}
	}
	UNPROTECT(1);
	return ans;
}

/*
 * TODO: Support "out of limits" views. An easy solution would be to trim the
 * views in R before 'x' is passed to XDoubleViews_viewMaxs().
 */
SEXP XDoubleViews_viewMaxs(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedDoubleSeq S, S_view;
	cachedIRanges cached_ranges;
	const double *S_view_elt;
	int i, j, ans_length, view_start, view_width, view_offset;
	double *ans_elt;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XDouble(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_NUMERIC(ans_length));
	for (i = 0, ans_elt = REAL(ans); i < ans_length; i++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewMaxs() doesn't support \"out of limits\" "
			      "views in XDoubleViews objects yet, sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		/* The code below does something *close* but not identical to
		 * what max() does on a standard double vector. See rmax()
		 * C static function in the R source code (src/main/summary.c)
		 * for what standard max() does. In particular, how they handle
		 * NA/NaN values seems slightly different. */
		*ans_elt = R_NegInf;
		for (j = 0, S_view_elt = S_view.seq;
		     j < S_view.length;
		     j++, S_view_elt++)
		{
			if (ISNAN(*S_view_elt)) { /* NA or NaN */
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = *S_view_elt;
					break;
				}
			} else if (*S_view_elt > *ans_elt)
				*ans_elt = *S_view_elt;
		}
	}
	UNPROTECT(1);
	return ans;
}

/*
 * TODO: Support "out of limits" views. An easy solution would be to trim the
 * views in R before 'x' is passed to XDoubleViews_viewSums().
 */
SEXP XDoubleViews_viewSums(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedDoubleSeq S, S_view;
	cachedIRanges cached_ranges;
	const double *S_view_elt;
	int i, j, ans_length, view_start, view_width, view_offset;
	double *ans_elt;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XDouble(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_NUMERIC(ans_length));
	for (i = 0, ans_elt = REAL(ans); i < ans_length; i++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewSums() doesn't support \"out of limits\" "
			      "views in XDoubleViews objects yet, sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		/* The code below mimics what sum() does on a standard double
		 * vector. See rsum() C static function in the R source code
		 * (src/main/summary.c) for what standard sum() does. */
		*ans_elt = 0;
		for (j = 0, S_view_elt = S_view.seq;
		     j < S_view.length;
		     j++, S_view_elt++)
		{
			if (LOGICAL(na_rm)[0] && ISNAN(*S_view_elt))
				continue;
			*ans_elt += *S_view_elt;
		}
	}
	UNPROTECT(1);
	return ans;
}

/*
 * TODO: Support "out of limits" views. Note that trimming 'x' in R before
 * it's passed to XDoubleViews_viewWhichMins() would not work here!
 */
SEXP XDoubleViews_viewWhichMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedDoubleSeq S, S_view;
	cachedIRanges cached_ranges;
	const double *S_view_elt;
	int i, j, ans_length, *ans_elt, view_start, view_width, view_offset;
	double cur_min;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XDouble(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewWhichMins() doesn't support \"out of "
			      "limits\" views in XDoubleViews objects yet, "
			      "sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		/* The code below does something *close* but not identical to
		 * what which.min() does on a standard double vector.
		 * TODO: See do_first_min() C function in the R source code
		 * (src/main/summary.c) for what standard which.min() does and
		 * maybe adjust the code below. */
		cur_min = R_PosInf;
		*ans_elt = view_start;
		for (j = 0, S_view_elt = S_view.seq;
		     j < S_view.length;
		     j++, S_view_elt++)
		{
			if (ISNAN(*S_view_elt)) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
					break;
				}
			} else if (*S_view_elt < cur_min) {
				cur_min = *S_view_elt;
				*ans_elt = view_start + j;
			}
		}
	}
	UNPROTECT(1);
	return ans;
}

/*
 * TODO: Support "out of limits" views. Note that trimming 'x' in R before
 * it's passed to XDoubleViews_viewWhichMaxs() would not work here!
 */
SEXP XDoubleViews_viewWhichMaxs(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedDoubleSeq S, S_view;
	cachedIRanges cached_ranges;
	const double *S_view_elt;
	int i, j, ans_length, *ans_elt, view_start, view_width, view_offset;
	double cur_max;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XDouble(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewWhichMaxs() doesn't support \"out of "
			      "limits\" views in XDoubleViews objects yet, "
			      "sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		/* The code below does something *close* but not identical to
		 * what which.max() does on a standard double vector.
		 * TODO: See do_first_min() C function in the R source code
		 * (src/main/summary.c) for what standard which.max() does and
		 * maybe adjust the code below. */
		cur_max = R_NegInf;
		*ans_elt = view_start;
		for (j = 0, S_view_elt = S_view.seq;
		     j < S_view.length;
		     j++, S_view_elt++)
		{
			if (ISNAN(*S_view_elt)) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
					break;
				}
			} else if (*S_view_elt > cur_max) {
				cur_max = *S_view_elt;
				*ans_elt = view_start + j;
			}
		}
	}
	UNPROTECT(1);
	return ans;
}
