/****************************************************************************
 *            Low-level manipulation of SimpleRangesList objects            *
 ****************************************************************************/
#include "IRanges.h"
#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)

/*
 * --- .Call ENTRY POINT ---
 */
SEXP SimpleIRangesList_isNormal(SEXP x, SEXP use_names)
{
	SEXP list_ir, ans, ans_names;
	IRanges_holder ir_holder;
	int x_len, i;

	list_ir = GET_SLOT(x, install("listData"));
	x_len = LENGTH(list_ir);
	PROTECT(ans = NEW_LOGICAL(x_len));
	for (i = 0; i < x_len; i++) {
		ir_holder = _hold_IRanges(VECTOR_ELT(list_ir, i));
		LOGICAL(ans)[i] = _is_normal_IRanges_holder(&ir_holder);
	}
	if (LOGICAL(use_names)[0]) {
		PROTECT(ans_names = duplicate(GET_NAMES(list_ir)));
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP SimpleNormalIRangesList_min(SEXP x)
{
	SEXP list_ir, ans, ans_names;
	IRanges_holder ir_holder;
	int x_len, ir_len, i;
	int *ans_elt;

	list_ir = GET_SLOT(x, install("listData"));
	x_len = LENGTH(list_ir);
	PROTECT(ans = NEW_INTEGER(x_len));
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++) {
		ir_holder = _hold_IRanges(VECTOR_ELT(list_ir, i));
		ir_len = _get_length_from_IRanges_holder(&ir_holder);
		if (ir_len == 0) {
			*ans_elt = INT_MAX;
		} else {
			*ans_elt = _get_start_elt_from_IRanges_holder(&ir_holder, 0);
		}
	}
	PROTECT(ans_names = duplicate(GET_NAMES(list_ir)));
	SET_NAMES(ans, ans_names);
	UNPROTECT(2);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP SimpleNormalIRangesList_max(SEXP x)
{
	SEXP list_ir, ans, ans_names;
	IRanges_holder ir_holder;
	int x_len, ir_len, i;
	int *ans_elt;

	list_ir = GET_SLOT(x, install("listData"));
	x_len = LENGTH(list_ir);
	PROTECT(ans = NEW_INTEGER(x_len));
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++) {
		ir_holder = _hold_IRanges(VECTOR_ELT(list_ir, i));
		ir_len = _get_length_from_IRanges_holder(&ir_holder);
		if (ir_len == 0) {
			*ans_elt = R_INT_MIN;
		} else {
			*ans_elt = _get_end_elt_from_IRanges_holder(&ir_holder, ir_len - 1);
		}
	}
	PROTECT(ans_names = duplicate(GET_NAMES(list_ir)));
	SET_NAMES(ans, ans_names);
	UNPROTECT(2);
	return ans;
}
