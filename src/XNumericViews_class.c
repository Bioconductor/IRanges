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
SEXP XNumericViews_slice(SEXP xdouble, SEXP lower, SEXP upper, SEXP include_lower, SEXP include_upper)
{
	SEXP x, ans, start, width;
	int i, x_length, ans_length;
	int *start_elt, *width_elt, curr_elt, prev_elt;
	double *x_elt, lower_elt, upper_elt;
	int (*lower_fun)(double, double);
	int (*upper_fun)(double, double);

	lower_fun = LOGICAL(include_lower)[0] ? &ge : &gt;
	upper_fun = LOGICAL(include_upper)[0] ? &le : &lt;

	lower_elt = REAL(lower)[0];
	upper_elt = REAL(upper)[0];

	x = _get_XSequence_tag(xdouble);
	x_length = LENGTH(x);
	ans_length = 0;
	prev_elt = 0;
	for (i = 1, x_elt = REAL(x); i <= x_length; i++, x_elt++) {
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
		for (i = 1, x_elt = REAL(x); i <= x_length; i++, x_elt++) {
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
	PROTECT(ans = _new_IRanges("XNumericViews", start, width, R_NilValue));
	SET_SLOT(ans, install("subject"), duplicate(xdouble));
	UNPROTECT(3);
	return ans;
}
