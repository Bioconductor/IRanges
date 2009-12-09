/****************************************************************************
 *              Low-level manipulation of GappedRanges objects              *
 ****************************************************************************/
#include "IRanges.h"


static const char *is_valid_GappedRanges_elt(const cachedIRanges *cached_ir)
{
	if (_get_cachedIRanges_length(cached_ir) == 0)
		return "IRanges object has no ranges";
	if (!_is_normal_cachedIRanges(cached_ir))
		return "IRanges object is not normal";
	return NULL;
}

/*
 * TODO: This is not needed anymore now that the 'cirl' slot has been
 * replaced by the 'cnirl' slot which is guaranteed to hold a
 * CompressedNormalIRangesList object (instead of just a CompressedIRangesList
 * object for the old slot). Hence the validity method for GappedRanges
 * should just check that all the elements in 'x@cnirl' are of length >= 1
 * (which can be done in R with elementLengths()).
 *
 * We assume that 'x@cnirl' is already a valid CompressedIRangesList object.
 * Here we only check that its elements are normal and of length >= 1.
 * ans_type: a single integer specifying the type of answer to return:
 *   0: 'ans' is a string describing the first validity failure or NULL;
 *   1: 'ans' is logical vector with TRUE values for valid elements in 'x'.
 */
SEXP valid_GappedRanges(SEXP x, SEXP ans_type)
{
	SEXP cnirl, ans;
	cachedCompressedIRangesList cached_cnirl;
	int x_length, ans_type0, i;
	cachedIRanges cached_ir;
	const char *errmsg;
	char string_buf[80];

	cnirl = GET_SLOT(x, install("cnirl"));
	cached_cnirl = _cache_CompressedIRangesList(cnirl);
	x_length = _get_cachedCompressedIRangesList_length(&cached_cnirl);
	ans_type0 = INTEGER(ans_type)[0];
	if (ans_type0 == 1)
		PROTECT(ans = NEW_LOGICAL(x_length));
	else
		ans = R_NilValue;
	for (i = 0; i < x_length; i++) {
		cached_ir = _get_cachedCompressedIRangesList_elt(&cached_cnirl, i);
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

