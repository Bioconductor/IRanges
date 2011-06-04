#include "IRanges.h"

#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XInteger_slice(SEXP x, SEXP lower, SEXP upper)
{
	SEXP x_tag, ans, start, width;
	int i, x_length, ans_length;
	int *x_elt, *start_elt, *width_elt;
	int lower_elt, upper_elt, curr_elt, prev_elt;

	lower_elt = INTEGER(lower)[0];
	upper_elt = INTEGER(upper)[0];

	x_tag = _get_XVector_tag(x);
	x_length = LENGTH(x_tag);
	ans_length = 0;
	prev_elt = 0;
	for (i = 1, x_elt = INTEGER(x_tag); i <= x_length; i++, x_elt++) {
		curr_elt = (*x_elt >= lower_elt) && (*x_elt <= upper_elt);
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
		for (i = 1, x_elt = INTEGER(x_tag); i <= x_length; i++, x_elt++) {
			curr_elt = (*x_elt >= lower_elt) && (*x_elt <= upper_elt);
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

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XIntegerViews_viewMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag;
	int i, j, ans_length, *ans_elt, *subject_elt, start, width;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		*ans_elt = INT_MAX;
		for (j = 0, subject_elt = INTEGER(subject_tag) + (start - 1);
		     j < width;
		     j++, subject_elt++)
		{
			if (*subject_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
					break;
				}
			} else if (*subject_elt < *ans_elt)
				*ans_elt = *subject_elt;
		}
	}
	UNPROTECT(1);
	return ans;
}


SEXP XIntegerViews_viewMaxs(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag;
	int i, j, ans_length, *ans_elt, *subject_elt, start, width;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		*ans_elt = INT_MIN;
		for (j = 0, subject_elt = INTEGER(subject_tag) + (start - 1);
		     j < width;
		     j++, subject_elt++)
		{
			if (*subject_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
					break;
				}
			} else if (*subject_elt > *ans_elt)
				*ans_elt = *subject_elt;
		}
	}
	UNPROTECT(1);
	return ans;
}


SEXP XIntegerViews_viewSums(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag;
	int i, j, ans_length, *ans_elt, *subject_elt, start, width;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		*ans_elt = 0;
		for (j = 0, subject_elt = INTEGER(subject_tag) + (start - 1);
		     j < width;
		     j++, subject_elt++)
		{
			if (*subject_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
					break;
				}
			}
			else
				*ans_elt += *subject_elt;
		}
		if (*ans_elt > INT_MAX || *ans_elt < R_INT_MIN)
			error("Integer overflow");
	}
	UNPROTECT(1);
	return ans;
}


SEXP XIntegerViews_viewWhichMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag;
	int i, j, ans_length, cur_min, *ans_elt, *subject_elt, start, width;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		cur_min = INT_MAX;
		*ans_elt = start;
		for (j = 0, subject_elt = INTEGER(subject_tag) + (start - 1);
		     j < width;
		     j++, subject_elt++)
		{
			if (*subject_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
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


SEXP XIntegerViews_viewWhichMaxs(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag;
	int i, j, ans_length, cur_max, *ans_elt, *subject_elt, start, width;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		cur_max = INT_MIN;
		*ans_elt = start;
		for (j = 0, subject_elt = INTEGER(subject_tag) + (start - 1);
		     j < width;
		     j++, subject_elt++)
		{
			if (*subject_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
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

