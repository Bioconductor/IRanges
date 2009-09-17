#include "IRanges.h"

#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XIntegerViews_viewMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag;
	int i, j, ans_length, *ans_elt, *subject_elt, start, width;
	cachedIRanges cached_x;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_x = _cache_IRanges(x);
	ans_length = _get_cachedIRanges_length(&cached_x);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_x, i);
		width = _get_cachedIRanges_elt_width(&cached_x, i);
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
	cachedIRanges cached_x;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_x = _cache_IRanges(x);
	ans_length = _get_cachedIRanges_length(&cached_x);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_x, i);
		width = _get_cachedIRanges_elt_width(&cached_x, i);
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
	cachedIRanges cached_x;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_x = _cache_IRanges(x);
	ans_length = _get_cachedIRanges_length(&cached_x);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_x, i);
		width = _get_cachedIRanges_elt_width(&cached_x, i);
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
	cachedIRanges cached_x;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_x = _cache_IRanges(x);
	ans_length = _get_cachedIRanges_length(&cached_x);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_x, i);
		width = _get_cachedIRanges_elt_width(&cached_x, i);
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
	cachedIRanges cached_x;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_XVector_tag(subject);

	cached_x = _cache_IRanges(x);
	ans_length = _get_cachedIRanges_length(&cached_x);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		start = _get_cachedIRanges_elt_start(&cached_x, i);
		width = _get_cachedIRanges_elt_width(&cached_x, i);
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
