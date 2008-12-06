#include "IRanges.h"

/*
 * --- .Call ENTRY POINT ---
 */
SEXP XRleIntegerViews_slice(SEXP x, SEXP lower, SEXP upper)
{
	SEXP ans, start, width;
	SEXP x_values_tag, x_lengths_tag;
	SEXP x_values, x_lengths;
	int i, index, x_len, ans_length;
	int *x_values_elt, *x_lengths_elt, *start_elt, *width_elt;
	int lower_elt, upper_elt, curr_elt, prev_elt;

	lower_elt = INTEGER(lower)[0];
	upper_elt = INTEGER(upper)[0];

	x_values = GET_SLOT(x, install("values"));
	x_lengths = GET_SLOT(x, install("lengths"));
	x_values_tag = _get_XSequence_tag(x_values);
	x_lengths_tag = _get_XSequence_tag(x_lengths);

	x_len = LENGTH(x_values_tag);
	ans_length = 0;
	prev_elt = 0;
	for (i = 1, x_values_elt = INTEGER(x_values_tag); i <= x_len; i++, x_values_elt++) {
		curr_elt = (*x_values_elt >= lower_elt) && (*x_values_elt <= upper_elt);
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
		index = 1;
		for (i = 1, x_values_elt = INTEGER(x_values_tag), x_lengths_elt = INTEGER(x_lengths_tag);
		     i <= x_len; i++, x_values_elt++, x_lengths_elt++) {
			curr_elt = (*x_values_elt >= lower_elt) && (*x_values_elt <= upper_elt);
			if (curr_elt) {
				if (prev_elt)
					*width_elt += *x_lengths_elt;
				else {
					start_elt++;
					width_elt++;
					*start_elt = index;
					*width_elt = *x_lengths_elt;
				}
			}
			prev_elt = curr_elt;
			index += *x_lengths_elt;
		}
	}
	PROTECT(ans = _new_IRanges("XRleIntegerViews", start, width, R_NilValue));
	SET_SLOT(ans, mkChar("subject"), duplicate(x));
	UNPROTECT(3);
	return ans;
}
