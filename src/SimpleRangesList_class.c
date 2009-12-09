/****************************************************************************
 *            Low-level manipulation of SimpleRangesList objects            *
 ****************************************************************************/
#include "IRanges.h"
#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)

/*
 * --- .Call ENTRY POINT ---
 */
SEXP SimpleIRangesList_isNormal(SEXP x)
{
	SEXP list_ir, ans, ans_names;
	cachedIRanges cached_ir;
	int x_length, i;

	list_ir = GET_SLOT(x, install("listData"));
	x_length = LENGTH(list_ir);
	PROTECT(ans = NEW_LOGICAL(x_length));
	for (i = 0; i < x_length; i++) {
		cached_ir = _cache_IRanges(VECTOR_ELT(list_ir, i));
		LOGICAL(ans)[i] = _is_normal_cachedIRanges(&cached_ir);
	}
	PROTECT(ans_names = duplicate(GET_NAMES(list_ir)));
	SET_NAMES(ans, ans_names);
	UNPROTECT(2);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP SimpleNormalIRangesList_min(SEXP x)
{
	SEXP list_ir, ans, ans_names;
	cachedIRanges cached_ir;
	int x_length, ir_length, i;
	int *ans_elt;

	list_ir = GET_SLOT(x, install("listData"));
	x_length = LENGTH(list_ir);
	PROTECT(ans = NEW_INTEGER(x_length));
	for (i = 0, ans_elt = INTEGER(ans); i < x_length; i++, ans_elt++) {
		cached_ir = _cache_IRanges(VECTOR_ELT(list_ir, i));
		ir_length = _get_cachedIRanges_length(&cached_ir);
		if (ir_length == 0) {
			*ans_elt = INT_MAX;
		} else {
			*ans_elt = _get_cachedIRanges_elt_start(&cached_ir, 0);
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
	cachedIRanges cached_ir;
	int x_length, ir_length, i;
	int *ans_elt;

	list_ir = GET_SLOT(x, install("listData"));
	x_length = LENGTH(list_ir);
	PROTECT(ans = NEW_INTEGER(x_length));
	for (i = 0, ans_elt = INTEGER(ans); i < x_length; i++, ans_elt++) {
		cached_ir = _cache_IRanges(VECTOR_ELT(list_ir, i));
		ir_length = _get_cachedIRanges_length(&cached_ir);
		if (ir_length == 0) {
			*ans_elt = R_INT_MIN;
		} else {
			*ans_elt = _get_cachedIRanges_elt_end(&cached_ir, ir_length - 1);
		}
	}
	PROTECT(ans_names = duplicate(GET_NAMES(list_ir)));
	SET_NAMES(ans, ans_names);
	UNPROTECT(2);
	return ans;
}
