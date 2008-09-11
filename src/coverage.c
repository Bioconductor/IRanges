#include "IRanges.h"

SEXP IRanges_coverage(SEXP x, SEXP weight, SEXP ans)
{
	int x_len, ans_len, *ans_elt, i1, i2, j;
	const int *x_start, *x_width, *weight_elt;
	SEXP tag;

	tag = _get_SequencePtr_tag(GET_SLOT(ans, install("xdata")));
	ans_len = LENGTH(tag);
	x_len = _get_IRanges_length(x);
	for (i1 = 0, x_start = _get_IRanges_start0(x),
	     x_width = _get_IRanges_width0(x),
	     i2 = 0, weight_elt = INTEGER(weight);
	     i1 < x_len;
	     i1++, x_start++, x_width++, i2++, weight_elt++)
	{
		if (i2 >= LENGTH(weight)) {
			/* recycle */
			i2 = 0;
			weight_elt = INTEGER(weight);
		}
		for (j = 0, ans_elt = INTEGER(tag) + *x_start - 1;
		     j < *x_width;
		     j++, ans_elt++)
		{
			*ans_elt += *weight_elt;
		}
	}
	return R_NilValue;
}

