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

/*
 * --- .Call ENTRY POINT ---
 * TODO: Support "out of limits" views. An easy solution would be to trim the
 * views in R before 'x' is passed to XIntegerViews_viewMins().
 */
SEXP XIntegerViews_viewMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	const int *S_view_elt;
	int i, j, ans_length, *ans_elt, view_start, view_width, view_offset;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewMins() doesn't support \"out of limits\" "
			      "views in XIntegerViews objects yet, sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		*ans_elt = INT_MAX;
		for (j = 0, S_view_elt = S_view.seq;
		     j < S_view.length;
		     j++, S_view_elt++)
		{
			if (*S_view_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
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
 * views in R before 'x' is passed to XIntegerViews_viewMaxs().
 */
SEXP XIntegerViews_viewMaxs(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	const int *S_view_elt;
	int i, j, ans_length, *ans_elt, view_start, view_width, view_offset;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewMaxs() doesn't support \"out of limits\" "
			      "views in XIntegerViews objects yet, sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		*ans_elt = INT_MIN;
		for (j = 0, S_view_elt = S_view.seq;
		     j < S_view.length;
		     j++, S_view_elt++)
		{
			if (*S_view_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
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
 * views in R before 'x' is passed to XIntegerViews_viewSums().
 */
SEXP XIntegerViews_viewSums(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	const int *S_view_elt;
	int i, j, ans_length, *ans_elt, view_start, view_width, view_offset;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
	cached_ranges = _cache_IRanges(GET_SLOT(x, install("ranges")));
	ans_length = _get_cachedIRanges_length(&cached_ranges);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		view_start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		view_width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		view_offset = view_start - 1;
		if (view_offset < 0 || view_offset + view_width > S.length) {
			UNPROTECT(1);
			error("viewSums() doesn't support \"out of limits\" "
			      "views in XIntegerViews objects yet, sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		*ans_elt = 0;
		for (j = 0, S_view_elt = S_view.seq;
		     j < S_view.length;
		     j++, S_view_elt++)
		{
			if (*S_view_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
					break;
				}
			}
			else
				*ans_elt += *S_view_elt;
		}
		/* HP: This of course doesn't work. FIXME */
		if (*ans_elt > INT_MAX || *ans_elt < R_INT_MIN)
			error("Integer overflow");
	}
	UNPROTECT(1);
	return ans;
}

/*
 * TODO: Support "out of limits" views. Note that trimming 'x' in R before
 * it's passed to XIntegerViews_viewWhichMins() would not work here!
 */
SEXP XIntegerViews_viewWhichMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	const int *S_view_elt;
	int i, j, ans_length, *ans_elt, view_start, view_width, view_offset,
	    cur_min;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
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
			      "limits\" views in XIntegerViews objects yet, "
			      "sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		cur_min = INT_MAX;
		*ans_elt = view_start;
		for (j = 0, S_view_elt = S_view.seq;
		     j < S_view.length;
		     j++, S_view_elt++)
		{
			if (*S_view_elt == NA_INTEGER) {
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
 * it's passed to XIntegerViews_viewWhichMaxs() would not work here!
 */
SEXP XIntegerViews_viewWhichMaxs(SEXP x, SEXP na_rm)
{
	SEXP ans, subject;
	cachedIntSeq S, S_view;
	cachedIRanges cached_ranges;
	const int *S_view_elt;
	int i, j, ans_length, *ans_elt, view_start, view_width, view_offset,
	    cur_max;

	subject = GET_SLOT(x, install("subject"));
	S = _cache_XInteger(subject);
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
			      "limits\" views in XIntegerViews objects yet, "
			      "sorry");
		}
		S_view.seq = S.seq + view_offset;
		S_view.length = view_width;
		cur_max = INT_MIN;
		*ans_elt = view_start;
		for (j = 0, S_view_elt = S_view.seq;
		     j < S_view.length;
		     j++, S_view_elt++)
		{
			if (*S_view_elt == NA_INTEGER) {
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

