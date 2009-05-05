#include "IRanges.h"


SEXP _get_CompressedIRangesList_elt(SEXP x, int at)
{
	int i, j, x_len, ans_len, shift;
	int *part_end_elt;
	SEXP part_end, unlistData, unlistData_names;
	SEXP ans, ans_start, ans_width, ans_names;

	part_end = GET_SLOT(GET_SLOT(x, install("partitioning")),
			                     install("end"));
	x_len = LENGTH(part_end);

	if (at < 0 || at >= x_len) {
		error("CompressedIRangesList element selection out of bounds");
	}

	unlistData = GET_SLOT(x, install("unlistData"));
	part_end_elt = INTEGER(part_end);

	if (at == 0)
		shift = 0;
	else
		shift = part_end_elt[at - 1];
	ans_len = part_end_elt[at] - shift;

	PROTECT(ans_start = NEW_INTEGER(ans_len));
	PROTECT(ans_width = NEW_INTEGER(ans_len));
	if (ans_len == 0) {
		PROTECT(ans_names = R_NilValue);
	} else {
		if (at == 0)
		shift = 0;
		for (i = 0; i < at; i++) {

		}
		memcpy(INTEGER(ans_start), (_get_IRanges_start0(unlistData) + shift),
			   ans_len * sizeof(int));
		memcpy(INTEGER(ans_width), (_get_IRanges_width0(unlistData) + shift),
			   ans_len * sizeof(int));

		unlistData_names = GET_SLOT(unlistData, install("NAMES"));
		if (unlistData_names == R_NilValue) {
			PROTECT(ans_names = R_NilValue);
		} else {
			PROTECT(ans_names = NEW_CHARACTER(ans_len));
			for (i = 0, j = shift; i < ans_len; i++, j++) {
				SET_STRING_ELT(ans_names, i, STRING_ELT(unlistData_names, j));
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
SEXP CompressedIRangesList_summary(SEXP object)
{
	int ans_len;
	SEXP part_end;
	SEXP ans, ans_names, col_names;

	part_end = GET_SLOT(GET_SLOT(object, install("partitioning")),
			                     install("end"));
	ans_len = LENGTH(part_end);
	PROTECT(ans = allocMatrix(INTSXP, ans_len, 2));
	memset(INTEGER(ans), 0, 2 * ans_len * sizeof(int));
	if (ans_len > 0) {
		int i, j, prev_end = 0;
		int *ans1_elt, *ans2_elt;
		const int *part_end_elt, *ranges_width;
		SEXP unlistData = GET_SLOT(object, install("unlistData"));
		ranges_width = _get_IRanges_width0(unlistData);
		for (i = 0, ans1_elt = INTEGER(ans), ans2_elt = (INTEGER(ans) + ans_len),
			 part_end_elt = INTEGER(part_end);
		     i < ans_len; i++, ans1_elt++, ans2_elt++, part_end_elt++)
		{
			*ans1_elt = *part_end_elt - prev_end;
			for (j = 0; j < *ans1_elt; j++) {
				*ans2_elt += *ranges_width;
				ranges_width++;
			}
			prev_end = *part_end_elt;
		}
	}
	PROTECT(ans_names = NEW_LIST(2));
	PROTECT(col_names = NEW_CHARACTER(2));
	SET_STRING_ELT(col_names, 0, mkChar("Length"));
	SET_STRING_ELT(col_names, 1, mkChar("WidthSum"));
	SET_ELEMENT(ans_names, 0,
			    duplicate(GET_SLOT(GET_SLOT(object, install("partitioning")),
			    		           install("NAMES"))));
	SET_ELEMENT(ans_names, 1, col_names);
	SET_DIMNAMES(ans, ans_names);
	UNPROTECT(3);
	return ans;
}
