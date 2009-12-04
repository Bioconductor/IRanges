/****************************************************************************
 *              Low-level manipulation of GappedRanges objects              *
 ****************************************************************************/
#include "IRanges.h"


SEXP GappedRanges_start(SEXP x)
{
	SEXP cirl, ans;
	cachedCompressedIRangesList cached_cirl;
	int ans_length, i, ir_length;
	cachedIRanges cached_ir;

	cirl = GET_SLOT(x, install("cirl"));
	cached_cirl = _cache_CompressedIRangesList(cirl);
	ans_length = _get_cachedCompressedIRangesList_length(&cached_cirl);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0; i < ans_length; i++) {
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
	int ans_length, i, ir_length;
	cachedIRanges cached_ir;

	cirl = GET_SLOT(x, install("cirl"));
	cached_cirl = _cache_CompressedIRangesList(cirl);
	ans_length = _get_cachedCompressedIRangesList_length(&cached_cirl);
	PROTECT(ans = NEW_INTEGER(ans_length));
	for (i = 0; i < ans_length; i++) {
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

