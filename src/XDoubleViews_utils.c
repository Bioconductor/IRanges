#include "IRanges.h"

#include <float.h>

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
	SEXP x_tag, ans, start, width;
	int i, x_length, ans_length;
	int *start_elt, *width_elt, curr_elt, prev_elt;
	double *x_elt, lower_elt, upper_elt;
	int (*lower_fun)(double, double);
	int (*upper_fun)(double, double);

	lower_fun = LOGICAL(include_lower)[0] ? &ge : &gt;
	upper_fun = LOGICAL(include_upper)[0] ? &le : &lt;

	lower_elt = REAL(lower)[0];
	upper_elt = REAL(upper)[0];

	x_tag = _get_XVector_tag(x);
	x_length = LENGTH(x_tag);
	ans_length = 0;
	prev_elt = 0;
	for (i = 1, x_elt = REAL(x_tag); i <= x_length; i++, x_elt++) {
		curr_elt = lower_fun(*x_elt, lower_elt) && upper_fun(*x_elt, upper_elt);
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
		for (i = 1, x_elt = REAL(x_tag); i <= x_length; i++, x_elt++) {
			curr_elt = lower_fun(*x_elt, lower_elt) && upper_fun(*x_elt, upper_elt);
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
 * These were copied wholesale from XInteverViews_utils.c and need translation
 */

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XDoubleViews_viewMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag;
	int i, j, ans_length, start, width;
	cachedIRanges cached_ranges;
    double *ans_elt, *subject_elt;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_NUMERIC(ans_length));
	for (i = 0, ans_elt = REAL(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		*ans_elt = DBL_MAX;
		for (j = 0, subject_elt = REAL(subject_tag) + (start - 1);
		     j < width;
		     j++, subject_elt++)
		{
			if (*subject_elt == NA_REAL) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_REAL;
					break;
				}
			} else if (*subject_elt < *ans_elt)
				*ans_elt = *subject_elt;
		}
	}
	UNPROTECT(1);
	return ans;
}


SEXP XDoubleViews_viewMaxs(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag;
	int i, j, ans_length, start, width;
    double *ans_elt, *subject_elt;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_NUMERIC(ans_length));
	for (i = 0, ans_elt = REAL(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		*ans_elt = DBL_MIN;
		for (j = 0, subject_elt = REAL(subject_tag) + (start - 1);
		     j < width;
		     j++, subject_elt++)
		{
			if (*subject_elt == NA_REAL) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_REAL;
					break;
				}
			} else if (*subject_elt > *ans_elt)
				*ans_elt = *subject_elt;
		}
	}
	UNPROTECT(1);
	return ans;
}


SEXP XDoubleViews_viewSums(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag;
	int i, j, ans_length, start, width;
    double *ans_elt, *subject_elt;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_NUMERIC(ans_length));
	for (i = 0, ans_elt = REAL(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		*ans_elt = 0;
		for (j = 0, subject_elt = REAL(subject_tag) + (start - 1);
		     j < width;
		     j++, subject_elt++)
		{
			if (*subject_elt == NA_REAL) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_REAL;
					break;
				}
			}
			else
				*ans_elt += *subject_elt;
		}
		/* do we need to check this here? -- steve
		if (*ans_elt > DBL_MAX || *ans_elt < R_INT_MIN)
			error("Integer overflow");
		*/
	}
	UNPROTECT(1);
	return ans;
}


SEXP XDoubleViews_viewWhichMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag;
	int i, j, ans_length, start, width, *ans_elt;
    double *subject_elt, cur_min;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		cur_min = DBL_MAX;
		*ans_elt = start;
		for (j = 0, subject_elt = REAL(subject_tag) + (start - 1);
		     j < width;
		     j++, subject_elt++)
		{
			if (*subject_elt == NA_REAL) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_REAL;
					break;
				}
			} else if (*subject_elt < cur_min) {
				cur_min = *subject_elt;
				*ans_elt = start + j;
			}
		}
	}
	UNPROTECT(1);
	return ans;
}


SEXP XDoubleViews_viewWhichMaxs(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag;
	int i, j, ans_length, *ans_elt, start, width;
    double cur_max, *subject_elt;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		cur_max = DBL_MIN;
		*ans_elt = start;
		for (j = 0, subject_elt = REAL(subject_tag) + (start - 1);
		     j < width;
		     j++, subject_elt++)
		{
			if (*subject_elt == NA_REAL) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_REAL;
					break;
				}
			} else if (*subject_elt > cur_max) {
				cur_max = *subject_elt;
				*ans_elt = start + j;
			}
		}
	}
	UNPROTECT(1);
	return ans;
}
