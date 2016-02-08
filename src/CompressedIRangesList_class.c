/****************************************************************************
 *         Low-level manipulation of CompressedIRangesList objects          *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"

#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)


/****************************************************************************
 * C-level abstract getters.
 */

CompressedIRangesList_holder _hold_CompressedIRangesList(SEXP x)
{
	CompressedIRangesList_holder x_holder;
	SEXP x_end;

	x_holder.classname = get_classname(x);
	x_end = _get_PartitioningByEnd_end(
			_get_CompressedList_partitioning(x));
	x_holder.length = LENGTH(x_end);
	x_holder.end = INTEGER(x_end);
	x_holder.unlistData_holder = _hold_IRanges(
			_get_CompressedList_unlistData(x));
	return x_holder;
}

int _get_length_from_CompressedIRangesList_holder(
		const CompressedIRangesList_holder *x_holder)
{
	return x_holder->length;
}

IRanges_holder _get_elt_from_CompressedIRangesList_holder(
		const CompressedIRangesList_holder *x_holder, int i)
{
	int offset, length;

	offset = i == 0 ? 0 : x_holder->end[i - 1];
	length = x_holder->end[i] - offset;
	return _get_linear_subset_from_IRanges_holder(
			&(x_holder->unlistData_holder),
			offset, length);
}

int _get_eltNROWS_from_CompressedIRangesList_holder(
		const CompressedIRangesList_holder *x_holder, int i)
{
/*
	IRanges_holder ir_holder;

	ir_holder = _get_elt_from_CompressedIRangesList_holder(x_holder, i);
	return _get_length_from_IRanges_holder(&ir_holder);
*/
	int offset;

	offset = i == 0 ? 0 : x_holder->end[i - 1];
	return x_holder->end[i] - offset; /* faster than the above */
}


/****************************************************************************
 * CompressedIRangesList methods.
 */

/* --- .Call ENTRY POINT --- */
SEXP CompressedIRangesList_isNormal(SEXP x, SEXP use_names)
{
	SEXP ans, ans_names;
	CompressedIRangesList_holder x_holder;
	IRanges_holder ir_holder;
	int x_len, i;

	x_holder = _hold_CompressedIRangesList(x);
	x_len = _get_length_from_CompressedIRangesList_holder(&x_holder);
	PROTECT(ans = NEW_LOGICAL(x_len));
	for (i = 0; i < x_len; i++) {
		ir_holder = _get_elt_from_CompressedIRangesList_holder(
					&x_holder, i);
		LOGICAL(ans)[i] = _is_normal_IRanges_holder(&ir_holder);
	}
	if (LOGICAL(use_names)[0]) {
		PROTECT(ans_names = duplicate(_get_CompressedList_names(x)));
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP CompressedIRangesList_summary(SEXP object)
{
	int ans_len;
	SEXP part_end;
	SEXP ans, ans_names, col_names;

	part_end = _get_PartitioningByEnd_end(
			_get_CompressedList_partitioning(object));
	ans_len = LENGTH(part_end);
	PROTECT(ans = allocMatrix(INTSXP, ans_len, 2));
	memset(INTEGER(ans), 0, 2 * ans_len * sizeof(int));
	if (ans_len > 0) {
		int i, j, prev_end = 0;
		int *ans1_elt, *ans2_elt;
		const int *part_end_elt, *ranges_width;
		SEXP unlistData = _get_CompressedList_unlistData(object);
		ranges_width = INTEGER(_get_IRanges_width(unlistData));
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
	SET_VECTOR_ELT(ans_names, 0,
			duplicate(_get_CompressedList_names(object)));
	SET_VECTOR_ELT(ans_names, 1, col_names);
	SET_DIMNAMES(ans, ans_names);
	UNPROTECT(3);
	return ans;
}


/****************************************************************************
 * CompressedNormalIRangesList methods.
 */

/* --- .Call ENTRY POINT --- */
SEXP CompressedNormalIRangesList_min(SEXP x, SEXP use_names)
{
	SEXP ans, ans_names;
	CompressedIRangesList_holder x_holder;
	IRanges_holder ir_holder;
	int x_len, ir_len, i;
	int *ans_elt;

	x_holder = _hold_CompressedIRangesList(x);
	x_len = _get_length_from_CompressedIRangesList_holder(&x_holder);
	PROTECT(ans = NEW_INTEGER(x_len));
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++) {
		ir_holder = _get_elt_from_CompressedIRangesList_holder(&x_holder, i);
		ir_len = _get_length_from_IRanges_holder(&ir_holder);
		if (ir_len == 0) {
			*ans_elt = INT_MAX;
		} else {
			*ans_elt = _get_start_elt_from_IRanges_holder(&ir_holder, 0);
		}
	}
	if (LOGICAL(use_names)[0]) {
		PROTECT(ans_names = duplicate(_get_CompressedList_names(x)));
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP CompressedNormalIRangesList_max(SEXP x, SEXP use_names)
{
	SEXP ans, ans_names;
	CompressedIRangesList_holder x_holder;
	IRanges_holder ir_holder;
	int x_len, ir_len, i;
	int *ans_elt;

	x_holder = _hold_CompressedIRangesList(x);
	x_len = _get_length_from_CompressedIRangesList_holder(&x_holder);
	PROTECT(ans = NEW_INTEGER(x_len));
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++) {
		ir_holder = _get_elt_from_CompressedIRangesList_holder(&x_holder, i);
		ir_len = _get_length_from_IRanges_holder(&ir_holder);
		if (ir_len == 0) {
			*ans_elt = R_INT_MIN;
		} else {
			*ans_elt = _get_end_elt_from_IRanges_holder(&ir_holder, ir_len - 1);
		}
	}
	if (LOGICAL(use_names)[0]) {
		PROTECT(ans_names = duplicate(_get_CompressedList_names(x)));
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

