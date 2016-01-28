/****************************************************************************
 *              Low-level manipulation of GappedRanges objects              *
 ****************************************************************************/
#include "IRanges.h"


static const char *is_valid_GappedRanges_elt(const IRanges_holder *ir_holder)
{
	if (_get_length_from_IRanges_holder(ir_holder) == 0)
		return "IRanges object has no ranges";
	if (!_is_normal_IRanges_holder(ir_holder))
		return "IRanges object is not normal";
	return NULL;
}

/*
 * TODO: This is not needed anymore now that the 'cirl' slot has been
 * replaced by the 'cnirl' slot which is guaranteed to hold a
 * CompressedNormalIRangesList object (instead of just a CompressedIRangesList
 * object for the old slot). Hence the validity method for GappedRanges
 * should just check that all the elements in 'x@cnirl' are of length >= 1
 * (which can be done in R with elementNROWS()).
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
	CompressedIRangesList_holder cnirl_holder;
	int x_len, ans_type0, i;
	IRanges_holder ir_holder;
	const char *errmsg;
	char string_buf[80];

	cnirl = GET_SLOT(x, install("cnirl"));
	cnirl_holder = _hold_CompressedIRangesList(cnirl);
	x_len = _get_length_from_CompressedIRangesList_holder(&cnirl_holder);
	ans_type0 = INTEGER(ans_type)[0];
	if (ans_type0 == 1)
		PROTECT(ans = NEW_LOGICAL(x_len));
	else
		ans = R_NilValue;
	for (i = 0; i < x_len; i++) {
		ir_holder = _get_elt_from_CompressedIRangesList_holder(&cnirl_holder, i);
		errmsg = is_valid_GappedRanges_elt(&ir_holder);
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

