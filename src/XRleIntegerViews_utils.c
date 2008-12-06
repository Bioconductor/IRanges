#include "IRanges.h"

#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XRleIntegerViews_viewMins(SEXP x, SEXP na_rm)
{
	int i, ans_length, index, lower_run, upper_run, upper_bound;
	int *ans_elt, *values_elt, *lengths_elt, *start_elt, *width_elt;
	SEXP ans, subject, values, values_tag, lengths, lengths_tag, start, width;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	values_tag = _get_XSequence_tag(values);
	lengths = GET_SLOT(subject, install("lengths"));
	lengths_tag = _get_XSequence_tag(lengths);
	start = GET_SLOT(x, install("start"));
	width = GET_SLOT(x, install("width"));

	ans_length = LENGTH(start);
	PROTECT(ans = NEW_INTEGER(ans_length));
	values_elt = INTEGER(values_tag);
	lengths_elt = INTEGER(lengths_tag);
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, ans_elt = INTEGER(ans), start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, ans_elt++, start_elt++, width_elt++)
	{
		*ans_elt = INT_MAX;
		while (index > 0 && upper_run > *start_elt) {
			upper_run -= *lengths_elt;
			values_elt--;
			lengths_elt--;
			index--;
		}
		while (upper_run < *start_elt) {
			values_elt++;
			lengths_elt++;
			index++;
			upper_run += *lengths_elt;
		}
		lower_run = upper_run - *lengths_elt + 1;
		upper_bound = *start_elt + *width_elt - 1;
		while (lower_run <= upper_bound) {
			if (*values_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
					break;
				}
			} else if (*values_elt < *ans_elt)
				*ans_elt = *values_elt;
			values_elt++;
			lengths_elt++;
			index++;
			lower_run = upper_run + 1;
			upper_run += *lengths_elt;
		}
	}
	UNPROTECT(1);
	return ans;
}


SEXP XRleIntegerViews_viewMaxs(SEXP x, SEXP na_rm)
{
	int i, ans_length, index, lower_run, upper_run, upper_bound;
	int *ans_elt, *values_elt, *lengths_elt, *start_elt, *width_elt;
	SEXP ans, subject, values, values_tag, lengths, lengths_tag, start, width;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	values_tag = _get_XSequence_tag(values);
	lengths = GET_SLOT(subject, install("lengths"));
	lengths_tag = _get_XSequence_tag(lengths);
	start = GET_SLOT(x, install("start"));
	width = GET_SLOT(x, install("width"));

	ans_length = LENGTH(start);
	PROTECT(ans = NEW_INTEGER(ans_length));
	values_elt = INTEGER(values_tag);
	lengths_elt = INTEGER(lengths_tag);
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, ans_elt = INTEGER(ans), start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, ans_elt++, start_elt++, width_elt++)
	{
		*ans_elt = INT_MIN;
		while (index > 0 && upper_run > *start_elt) {
			upper_run -= *lengths_elt;
			values_elt--;
			lengths_elt--;
			index--;
		}
		while (upper_run < *start_elt) {
			values_elt++;
			lengths_elt++;
			index++;
			upper_run += *lengths_elt;
		}
		lower_run = upper_run - *lengths_elt + 1;
		upper_bound = *start_elt + *width_elt - 1;
		while (lower_run <= upper_bound) {
			if (*values_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
					break;
				}
			} else if (*values_elt > *ans_elt)
				*ans_elt = *values_elt;
			values_elt++;
			lengths_elt++;
			index++;
			lower_run = upper_run + 1;
			upper_run += *lengths_elt;
		}
	}
	UNPROTECT(1);
	return ans;
}


SEXP XRleIntegerViews_viewSums(SEXP x, SEXP na_rm)
{
	int i, ans_length, index, lower_run, upper_run, lower_bound, upper_bound;
	int *ans_elt, *values_elt, *lengths_elt, *start_elt, *width_elt;
	SEXP ans, subject, values, values_tag, lengths, lengths_tag, start, width;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	values_tag = _get_XSequence_tag(values);
	lengths = GET_SLOT(subject, install("lengths"));
	lengths_tag = _get_XSequence_tag(lengths);
	start = GET_SLOT(x, install("start"));
	width = GET_SLOT(x, install("width"));

	ans_length = LENGTH(start);
	PROTECT(ans = NEW_INTEGER(ans_length));
	values_elt = INTEGER(values_tag);
	lengths_elt = INTEGER(lengths_tag);
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, ans_elt = INTEGER(ans), start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, ans_elt++, start_elt++, width_elt++)
	{
		*ans_elt = 0;
		while (index > 0 && upper_run > *start_elt) {
			upper_run -= *lengths_elt;
			values_elt--;
			lengths_elt--;
			index--;
		}
		while (upper_run < *start_elt) {
			values_elt++;
			lengths_elt++;
			index++;
			upper_run += *lengths_elt;
		}
		lower_run = upper_run - *lengths_elt + 1;
		lower_bound = *start_elt;
		upper_bound = *start_elt + *width_elt - 1;
		while (lower_run <= upper_bound) {
			if (*values_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
					break;
				}
			} else
				*ans_elt += *values_elt *
				    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
				         (lower_bound > lower_run ? lower_bound : lower_run));
			values_elt++;
			lengths_elt++;
			index++;
			lower_run = upper_run + 1;
			lower_bound = lower_run;
			upper_run += *lengths_elt;
		}
		if (*ans_elt > INT_MAX || *ans_elt < R_INT_MIN)
			error("Integer overflow");
	}
	UNPROTECT(1);
	return ans;
}


SEXP XRleIntegerViews_viewWhichMins(SEXP x, SEXP na_rm)
{
	int i, ans_length, index, lower_run, upper_run, lower_bound, upper_bound;
	int cur_min, *ans_elt, *values_elt, *lengths_elt, *start_elt, *width_elt;
	SEXP ans, subject, values, values_tag, lengths, lengths_tag, start, width;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	values_tag = _get_XSequence_tag(values);
	lengths = GET_SLOT(subject, install("lengths"));
	lengths_tag = _get_XSequence_tag(lengths);
	start = GET_SLOT(x, install("start"));
	width = GET_SLOT(x, install("width"));

	ans_length = LENGTH(start);
	PROTECT(ans = NEW_INTEGER(ans_length));
	values_elt = INTEGER(values_tag);
	lengths_elt = INTEGER(lengths_tag);
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, ans_elt = INTEGER(ans), start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, ans_elt++, start_elt++, width_elt++)
	{
		cur_min = INT_MAX;
		*ans_elt = *start_elt;
		while (index > 0 && upper_run > *start_elt) {
			upper_run -= *lengths_elt;
			values_elt--;
			lengths_elt--;
			index--;
		}
		while (upper_run < *start_elt) {
			values_elt++;
			lengths_elt++;
			index++;
			upper_run += *lengths_elt;
		}
		lower_run = upper_run - *lengths_elt + 1;
		lower_bound = *start_elt;
		upper_bound = *start_elt + *width_elt - 1;
		while (lower_run <= upper_bound) {
			if (*values_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
					break;
				}
			} else if (*values_elt < cur_min) {
				cur_min = *values_elt;
				*ans_elt = lower_bound;
			}
			values_elt++;
			lengths_elt++;
			index++;
			lower_run = upper_run + 1;
			lower_bound = lower_run;
			upper_run += *lengths_elt;
		}
	}
	UNPROTECT(1);
	return ans;
}


SEXP XRleIntegerViews_viewWhichMaxs(SEXP x, SEXP na_rm)
{
	int i, ans_length, index, lower_run, upper_run, lower_bound, upper_bound;
	int cur_max, *ans_elt, *values_elt, *lengths_elt, *start_elt, *width_elt;
	SEXP ans, subject, values, values_tag, lengths, lengths_tag, start, width;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	values_tag = _get_XSequence_tag(values);
	lengths = GET_SLOT(subject, install("lengths"));
	lengths_tag = _get_XSequence_tag(lengths);
	start = GET_SLOT(x, install("start"));
	width = GET_SLOT(x, install("width"));

	ans_length = LENGTH(start);
	PROTECT(ans = NEW_INTEGER(ans_length));
	values_elt = INTEGER(values_tag);
	lengths_elt = INTEGER(lengths_tag);
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, ans_elt = INTEGER(ans), start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, ans_elt++, start_elt++, width_elt++)
	{
		cur_max = INT_MIN;
		*ans_elt = *start_elt;
		while (index > 0 && upper_run > *start_elt) {
			upper_run -= *lengths_elt;
			values_elt--;
			lengths_elt--;
			index--;
		}
		while (upper_run < *start_elt) {
			values_elt++;
			lengths_elt++;
			index++;
			upper_run += *lengths_elt;
		}
		lower_run = upper_run - *lengths_elt + 1;
		lower_bound = *start_elt;
		upper_bound = *start_elt + *width_elt - 1;
		while (lower_run <= upper_bound) {
			if (*values_elt == NA_INTEGER) {
				if (!LOGICAL(na_rm)[0]) {
					*ans_elt = NA_INTEGER;
					break;
				}
			} else if (*values_elt > cur_max) {
				cur_max = *values_elt;
				*ans_elt = lower_bound;
			}
			values_elt++;
			lengths_elt++;
			index++;
			lower_run = upper_run + 1;
			lower_bound = lower_run;
			upper_run += *lengths_elt;
		}
	}
	UNPROTECT(1);
	return ans;
}
