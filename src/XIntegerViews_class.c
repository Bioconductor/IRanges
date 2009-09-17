#include "IRanges.h"

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XIntegerViews_slice(SEXP xint, SEXP lower, SEXP upper)
{
	SEXP x, ans, start, width;
	int i, x_length, ans_length;
	int *x_elt, *start_elt, *width_elt;
	int lower_elt, upper_elt, curr_elt, prev_elt;

	lower_elt = INTEGER(lower)[0];
	upper_elt = INTEGER(upper)[0];

	x = _get_XVector_tag(xint);
	x_length = LENGTH(x);
	ans_length = 0;
	prev_elt = 0;
	for (i = 1, x_elt = INTEGER(x); i <= x_length; i++, x_elt++) {
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
		for (i = 1, x_elt = INTEGER(x); i <= x_length; i++, x_elt++) {
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
	PROTECT(ans = _new_IRanges("XIntegerViews", start, width, R_NilValue));
	SET_SLOT(ans, install("subject"), duplicate(xint));
	UNPROTECT(3);
	return ans;
}
