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


/*
 * Do NOT try to make this a .Call() entry point!
 * Its arguments are NOT duplicated so it would be a disaster if they were
 * coming from the user space.
 */
SEXP _new_XSequence(const char *classname, SEXP xdata, int offset, int length)
{
	SEXP classdef, ans;

	classdef = MAKE_CLASS(classname);
	PROTECT(ans = NEW_OBJECT(classdef));
	SET_SLOT(ans, mkChar("xdata"), xdata);
	SET_SLOT(ans, mkChar("offset"), ScalarInteger(offset));
	SET_SLOT(ans, mkChar("length"), ScalarInteger(length));
	UNPROTECT(1);
	return ans;
}

