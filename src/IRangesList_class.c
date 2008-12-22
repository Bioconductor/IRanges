#include "IRanges.h"


SEXP _get_IRangesList_elt(SEXP x, int at)
{
	int x_len, list_index, shift, ans_len;
	const int *comp_ind_elt;
	SEXP list, list_elt, list_elt_names, cum_len, comp_ind;
	SEXP ans, ans_start, ans_width, ans_names;

	list = GET_SLOT(x, install("elements"));
	cum_len = GET_SLOT(x, install("elementCumLengths"));
	comp_ind = GET_SLOT(x, install("compressedIndices"));
	comp_ind_elt = INTEGER(comp_ind);

	list_index = 0;
	list_elt = VECTOR_ELT(list, 0);
	x_len = LENGTH(cum_len) - 1;
	if (at < 0 || at >= x_len) {
		error("IRangesList element selection out of bounds");
	}

	ans_len = INTEGER(cum_len)[at+1] - INTEGER(cum_len)[at];

	PROTECT(ans_start = NEW_INTEGER(ans_len));
	PROTECT(ans_width = NEW_INTEGER(ans_len));
	if (ans_len == 0) {
		PROTECT(ans_names = R_NilValue);
	} else {
		while(at >= *(comp_ind_elt+1)) {
			comp_ind_elt++;
			list_index++;
		}

		list_elt = VECTOR_ELT(list, list_index);
		shift = INTEGER(cum_len)[at] - INTEGER(cum_len)[*comp_ind_elt - 1];

		memcpy(INTEGER(ans_start), (_get_IRanges_start0(list_elt) + shift),
			   ans_len * sizeof(int));
		memcpy(INTEGER(ans_width), (_get_IRanges_width0(list_elt) + shift),
			   ans_len * sizeof(int));

		list_elt_names = _get_IRanges_names(list_elt);
		if (list_elt_names == R_NilValue) {
			PROTECT(ans_names = R_NilValue);
		} else {
			int i, j;
			PROTECT(ans_names = NEW_CHARACTER(ans_len));
			for (i = 0, j = shift; i < ans_len; i++, j++) {
				SET_STRING_ELT(ans_names, i, STRING_ELT(list_elt_names, j));
			}
		}
	}
	PROTECT(ans = _new_IRanges("IRanges", ans_start, ans_width, ans_names));
	UNPROTECT(4);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP IRangesList_summary(SEXP object)
{
	int i, j, list_index, shift, ans_len, *ans1_elt, *ans2_elt;
	const int *list_elt_width, *cum_len_elt, *comp_ind_elt;
	SEXP list, list_elt, cum_len, comp_ind;
	SEXP ans, ans_names, col_names;

	list = GET_SLOT(object, install("elements"));
	cum_len = GET_SLOT(object, install("elementCumLengths"));
	comp_ind = GET_SLOT(object, install("compressedIndices"));
	comp_ind_elt = INTEGER(comp_ind);

	list_index = 0;
	list_elt = VECTOR_ELT(list, 0);
	ans_len = LENGTH(cum_len) - 1;
	PROTECT(ans = allocMatrix(INTSXP, ans_len, 2));
	memset(INTEGER(ans), 0, 2 * ans_len * sizeof(int));
	for (i = 1, ans1_elt = INTEGER(ans), ans2_elt = (INTEGER(ans) + ans_len),
		 cum_len_elt = INTEGER(cum_len);
	     i <= ans_len; i++, ans1_elt++, ans2_elt++, cum_len_elt++)
	{
		*ans1_elt = *(cum_len_elt+1) - *cum_len_elt;
		if (*ans1_elt > 0) {
			if (i >= *(comp_ind_elt+1)) {
				while(i >= *(comp_ind_elt+1)) {
					comp_ind_elt++;
					list_index++;
				}
				list_elt = VECTOR_ELT(list, list_index);
			}
			shift = *cum_len_elt - INTEGER(cum_len)[*comp_ind_elt - 1];
			for (j = 0, list_elt_width = (_get_IRanges_width0(list_elt) + shift);
			     j < *ans1_elt;
			     j++, list_elt_width++)
			{
				*ans2_elt += *list_elt_width;
			}
		}
	}
	PROTECT(ans_names = NEW_LIST(2));
	PROTECT(col_names = NEW_CHARACTER(2));
	SET_STRING_ELT(col_names, 0, mkChar("Length"));
	SET_STRING_ELT(col_names, 1, mkChar("WidthSum"));
	SET_ELEMENT(ans_names, 0, duplicate(GET_SLOT(object, install("NAMES"))));
	SET_ELEMENT(ans_names, 1, col_names);
	SET_DIMNAMES(ans, ans_names);
	UNPROTECT(3);
	return ans;
}
