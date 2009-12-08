/****************************************************************************
 *              Low-level manipulation of GappedRanges objects              *
 ****************************************************************************/
#include "IRanges.h"


static const char *is_valid_GappedRanges_elt(const cachedIRanges *cached_ir)
{
	int ir_length, i;

	ir_length = _get_cachedIRanges_length(cached_ir);
	if (ir_length == 0)
		return "IRanges object has no ranges";
	for (i = 1; i < ir_length; i++) {
		if (_get_cachedIRanges_elt_start(cached_ir, i)
		 <= _get_cachedIRanges_elt_end(cached_ir, i - 1) + 1)
			return "IRanges object is not normal";
	}
	return NULL;
}

/* We assume that 'x@cirl' is already a valid CompressedIRangesList object.
 * Here we only check that its elements are normal and of length >= 1.
 * ans_type: a single integer specifying the type of answer to return:
 *   0: 'ans' is a string describing the first validity failure or NULL;
 *   1: 'ans' is logical vector with TRUE values for valid elements in 'x'.
 */
SEXP valid_GappedRanges(SEXP x, SEXP ans_type)
{
	SEXP cirl, ans;
	cachedCompressedIRangesList cached_cirl;
	int x_length, ans_type0, i;
	cachedIRanges cached_ir;
	const char *errmsg;
	char string_buf[80];

	cirl = GET_SLOT(x, install("cirl"));
	cached_cirl = _cache_CompressedIRangesList(cirl);
	x_length = _get_cachedCompressedIRangesList_length(&cached_cirl);
	ans_type0 = INTEGER(ans_type)[0];
	if (ans_type0 == 1)
		PROTECT(ans = NEW_LOGICAL(x_length));
	else
		ans = R_NilValue;
	for (i = 0; i < x_length; i++) {
		cached_ir = _get_cachedCompressedIRangesList_elt(&cached_cirl, i);
		errmsg = is_valid_GappedRanges_elt(&cached_ir);
		if (ans_type0 == 1) {
			LOGICAL(ans)[i] = errmsg == NULL;
			continue;
		}
		if (errmsg != NULL) {
			snprintf(string_buf, sizeof(string_buf),
				 "element %d is invalid (%s)", i + 1, errmsg);
			return mkString(string_buf);
		}
	}
	if (ans_type0 == 1)
		UNPROTECT(1);
	return ans;
}

SEXP GappedRanges_start(SEXP x)
{
	SEXP cirl, ans;
	cachedCompressedIRangesList cached_cirl;
	int x_length, i, ir_length;
	cachedIRanges cached_ir;

	cirl = GET_SLOT(x, install("cirl"));
	cached_cirl = _cache_CompressedIRangesList(cirl);
	x_length = _get_cachedCompressedIRangesList_length(&cached_cirl);
	PROTECT(ans = NEW_INTEGER(x_length));
	for (i = 0; i < x_length; i++) {
		cached_ir = _get_cachedCompressedIRangesList_elt(&cached_cirl, i);
		ir_length = _get_cachedIRanges_length(&cached_ir);
		if (ir_length == 0) {
			UNPROTECT(1);
			error("IRanges internal error in GappedRanges_start(): "
			      "element %d in 'x' has length 0");
		}
		INTEGER(ans)[i] = _get_cachedIRanges_elt_start(&cached_ir, 0);
	}
	UNPROTECT(1);
	return ans;
}

SEXP GappedRanges_end(SEXP x)
{
	SEXP cirl, ans;
	cachedCompressedIRangesList cached_cirl;
	int x_length, i, ir_length;
	cachedIRanges cached_ir;

	cirl = GET_SLOT(x, install("cirl"));
	cached_cirl = _cache_CompressedIRangesList(cirl);
	x_length = _get_cachedCompressedIRangesList_length(&cached_cirl);
	PROTECT(ans = NEW_INTEGER(x_length));
	for (i = 0; i < x_length; i++) {
		cached_ir = _get_cachedCompressedIRangesList_elt(&cached_cirl, i);
		ir_length = _get_cachedIRanges_length(&cached_ir);
		if (ir_length == 0) {
			UNPROTECT(1);
			error("IRanges internal error in GappedRanges_end(): "
			      "element %d in 'x' has length 0");
		}
		INTEGER(ans)[i] = _get_cachedIRanges_elt_end(&cached_ir, ir_length - 1);
	}
	UNPROTECT(1);
	return ans;
}

