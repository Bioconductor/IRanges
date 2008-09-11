#include "IRanges.h"

static int gt(int x, int y) {
	return x > y;
}

static int lt(int x, int y) {
	return x < y;
}

static int ge(int x, int y) {
	return x >= y;
}

static int le(int x, int y) {
	return x <= y;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XIntegerViews_slice(SEXP xint, SEXP lower, SEXP upper, SEXP include_lower, SEXP include_upper)
{
	SEXP x, ans, start, width;
	int i, x_length, ans_length;
	int *x_elt, *start_elt, *width_elt;
	int lower_elt, upper_elt, curr_elt, prev_elt;
	int (*lower_fun)(int, int);
	int (*upper_fun)(int, int);

	lower_fun = LOGICAL(include_lower)[0] ? &ge : &gt;
	upper_fun = LOGICAL(include_upper)[0] ? &le : &lt;

	lower_elt = INTEGER(lower)[0];
	upper_elt = INTEGER(upper)[0];

	x = _get_VectorPtr_tag(GET_SLOT(xint, install("xdata")));
	x_length = LENGTH(x);
	ans_length = 0;
	prev_elt = 0;
	for (i = 1, x_elt = INTEGER(x); i <= x_length; i++, x_elt++) {
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
		for (i = 1, x_elt = INTEGER(x); i <= x_length; i++, x_elt++) {
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
	PROTECT(ans = _new_IRanges("XIntegerViews", start, width, R_NilValue));
	SET_SLOT(ans, mkChar("subject"), duplicate(xint));
	UNPROTECT(3);
	return ans;
}

