#include "IRanges.h"

/*
 * --- .Call ENTRY POINT ---
 */
SEXP summary_IRangesList(SEXP object)
{
	int x_len, *ans1_elt, *ans2_elt, i, j;
	const int *x_elt_width;
	SEXP x, x_elt, ans, ans_names, col_names;

	x = GET_SLOT(object, install("elements"));
	x_len = LENGTH(x);
	PROTECT(ans = allocMatrix(INTSXP, x_len, 2));
	memset(INTEGER(ans), 0, 2 * x_len * sizeof(int));
	for (i = 0, ans1_elt = INTEGER(ans), ans2_elt = INTEGER(ans) + x_len;
	     i < x_len;
	     i++, ans1_elt++, ans2_elt++)
	{
		x_elt = VECTOR_ELT(x, i);
		*ans1_elt = LENGTH(_get_IRanges_width(x_elt));
		for (j = 0, x_elt_width = _get_IRanges_width0(x_elt);
		     j < *ans1_elt;
		     j++, x_elt_width++)
		{
			*ans2_elt += *x_elt_width;
		}
	}
	PROTECT(ans_names = NEW_LIST(2));
	PROTECT(col_names = NEW_CHARACTER(2));
	SET_STRING_ELT(col_names, 0, mkChar("Length"));
	SET_STRING_ELT(col_names, 1, mkChar("WidthSum"));
	SET_ELEMENT(ans_names, 0, GET_SLOT(object, install("NAMES")));
	SET_ELEMENT(ans_names, 1, col_names);
	SET_DIMNAMES(ans, ans_names);
	UNPROTECT(3);
	return ans;
}
