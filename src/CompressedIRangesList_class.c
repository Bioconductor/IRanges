#include "IRanges.h"

cachedIRanges _cache_CompressedIRangesList_elt(SEXP x, int i)
{
	SEXP x_part_end, x_unlistData;
	int offset, length;

	x_part_end = GET_SLOT(GET_SLOT(x, install("partitioning")), install("end"));
	x_unlistData = GET_SLOT(x, install("unlistData"));
	offset = i == 0 ? 0 : INTEGER(x_part_end)[i - 1];
	length = INTEGER(x_part_end)[i] - offset;
	return _cache_subIRanges(x_unlistData, offset, length);
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
