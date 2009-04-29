/****************************************************************************
 *               Low-level manipulation of XSequence objects                *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_XSequence_class()
{
#ifdef DEBUG_IRANGES
	debug = !debug;
	Rprintf("Debug mode turned %s in file %s\n",
		debug ? "on" : "off", __FILE__);
#else
	Rprintf("Debug mode not available in file %s\n", __FILE__);
#endif
	return R_NilValue;
}


/****************************************************************************
 * C-level accessor functions for XSequence objects.
 *
 * Be careful that these functions do NOT duplicate the returned slot.
 * Thus they cannot be made .Call() entry points!
 */

SEXP _get_XSequence_xdata(SEXP x)
{
	return GET_SLOT(x, install("xdata"));
}

SEXP _get_XSequence_tag(SEXP x)
{
	return _get_SequencePtr_tag(_get_XSequence_xdata(x));
}

SEXP _get_XSequence_offset(SEXP x)
{
	return GET_SLOT(x, install("offset"));
}

SEXP _get_XSequence_length(SEXP x)
{
	return GET_SLOT(x, install("length"));
}


/****************************************************************************
 * C-level constructor functions for XSequence objects.
 *
 * Be careful that these functions do NOT duplicate their arguments before
 * they put them in the slots of the returned objects.
 * Thus they cannot be made .Call() entry points!
 */

SEXP _new_XSequence(const char *classname, SEXP xdata, int offset, int length)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));
	SET_SLOT(ans, mkChar("xdata"), xdata);
	SET_SLOT(ans, mkChar("offset"), ScalarInteger(offset));
	SET_SLOT(ans, mkChar("length"), ScalarInteger(length));
	UNPROTECT(2);
	return ans;
}

SEXP _new_XRaw_from_tag(const char *classname, SEXP tag)
{
	SEXP xdata, ans;

	if (!IS_RAW(tag))
		error("IRanges internal error in _new_XRaw_from_tag(): "
		      "'tag' is not RAW");
	PROTECT(xdata = _new_SequencePtr("RawPtr", tag));
	PROTECT(ans = _new_XSequence(classname, xdata, 0, LENGTH(tag)));
	UNPROTECT(2);
	return ans;
}

SEXP _new_XInteger_from_tag(const char *classname, SEXP tag)
{
	SEXP xdata, ans;

	if (!IS_INTEGER(tag))
		error("IRanges internal error in _new_XInteger_from_tag(): "
		      "'tag' is not INTEGER");
	PROTECT(xdata = _new_SequencePtr("IntegerPtr", tag));
	PROTECT(ans = _new_XSequence(classname, xdata, 0, LENGTH(tag)));
	UNPROTECT(2);
	return ans;
}

SEXP _new_XNumeric_from_tag(const char *classname, SEXP tag)
{
	SEXP xdata, ans;

	if (!IS_NUMERIC(tag))
		error("IRanges internal error in _new_XNumeric_from_tag(): "
		      "'tag' is not NUMERIC");
	PROTECT(xdata = _new_SequencePtr("NumericPtr", tag));
	PROTECT(ans = _new_XSequence(classname, xdata, 0, LENGTH(tag)));
	UNPROTECT(2);
	return ans;
}

