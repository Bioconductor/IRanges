#include "IRanges.h"

#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XIntegerViews_viewMins(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag, start, width;
	int i, j, ans_length, *ans_elt, *subject_elt, *start_elt, *width_elt;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_VectorPtr_tag(GET_SLOT(subject, install("xdata")));
	start = GET_SLOT(x, install("start"));
	width = GET_SLOT(x, install("width"));

	ans_length = LENGTH(start);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans), start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, ans_elt++, start_elt++, width_elt++)
	{
		*ans_elt = INT_MAX;
		for (j = 0, subject_elt = INTEGER(subject_tag) + (*start_elt - 1);
		     j < *width_elt;
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

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XIntegerViews_viewMaxs(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag, start, width;
	int i, j, ans_length, *ans_elt, *subject_elt, *start_elt, *width_elt;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_VectorPtr_tag(GET_SLOT(subject, install("xdata")));
	start = GET_SLOT(x, install("start"));
	width = GET_SLOT(x, install("width"));

	ans_length = LENGTH(start);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans), start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, ans_elt++, start_elt++, width_elt++)
	{
		*ans_elt = INT_MIN;
		for (j = 0, subject_elt = INTEGER(subject_tag) + (*start_elt - 1);
		     j < *width_elt;
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

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XIntegerViews_viewSums(SEXP x, SEXP na_rm)
{
	SEXP ans, subject, subject_tag, start, width;
	int i, j, ans_length, *ans_elt, *subject_elt, *start_elt, *width_elt;

	subject = GET_SLOT(x, install("subject"));
	subject_tag = _get_VectorPtr_tag(GET_SLOT(subject, install("xdata")));
	start = GET_SLOT(x, install("start"));
	width = GET_SLOT(x, install("width"));

	ans_length = LENGTH(start);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0, ans_elt = INTEGER(ans), start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, ans_elt++, start_elt++, width_elt++)
	{
		*ans_elt = 0;
		for (j = 0, subject_elt = INTEGER(subject_tag) + (*start_elt - 1);
		     j < *width_elt;
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

