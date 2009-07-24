/****************************************************************************
 *              Low-level manipulation of SequencePtr objects               *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_SequencePtr_class()
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

/*
 * Print some obscure info about an "externalptr" object.
 * From R:
 *   .Call("ExternalPtr_show", new("externalptr"), PACKAGE="IRanges")
 */
SEXP ExternalPtr_show(SEXP xp)
{
	SEXP s;
	void *p;

	Rprintf("Object of class 'externalptr':\n");
	Rprintf("  xp adress: %p\n", xp);
	p = R_ExternalPtrAddr(xp);
	Rprintf("  R_ExternalPtrAddr(xp): %p\n", p);
	s = R_ExternalPtrTag(xp);
	Rprintf("  R_ExternalPtrTag(xp): %p", s);
	Rprintf("%s\n", TYPEOF(s) == NILSXP ? " (NILSXP)" : "");
	s = R_ExternalPtrProtected(xp);
	Rprintf("  R_ExternalPtrProtected(xp): %p", s);
	Rprintf("%s\n", TYPEOF(s) == NILSXP ? " (NILSXP)" : "");
	return R_NilValue;
}

/*
 * new("externalptr") will always return the same instance of an external
 * pointer object! If you need a new instance, use this function instead.
 * From R:
 *   xp <- .Call("ExternalPtr_new", PACKAGE="IRanges")
 */
SEXP ExternalPtr_new()
{
	return R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);
}


/****************************************************************************
 * C-level getters.
 */

static SEXP xp_symbol = NULL;

/*
 * Be careful that this function does NOT duplicate the returned SEXP.
 * Thus it cannot be made a .Call() entry point!
 */
SEXP _get_SequencePtr_tag(SEXP x)
{
	INIT_STATIC_SYMBOL(xp)
	return R_ExternalPtrTag(GET_SLOT(x, xp_symbol));
}

int _get_SequencePtr_length(SEXP x)
{
	return LENGTH(_get_SequencePtr_tag(x));
}


SEXP SequencePtr_length(SEXP x)
{
	return ScalarInteger(_get_SequencePtr_length(x));
}


/****************************************************************************
 * C-level setters.
 *
 * Be careful that these functions do NOT duplicate the assigned value!
 */

static void set_SequencePtr_tag(SEXP x, SEXP value)
{
	SEXP xp;

	PROTECT(xp = R_MakeExternalPtr(NULL, value, R_NilValue));
	INIT_STATIC_SYMBOL(xp)
	SET_SLOT(x, xp_symbol, xp);
	UNPROTECT(1);
	return;
}


/****************************************************************************
 * C-level constructors.
 *
 * Be careful that these functions do NOT duplicate their arguments before
 * putting them in the slots of the returned object.
 * Thus they cannot be made .Call() entry points!
 */

/* 'classname' can be "RawPtr", "IntegerPtr" or "NumericPtr" */
SEXP _new_SequencePtr(const char *classname, SEXP tag)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));
	set_SequencePtr_tag(ans, tag);
	UNPROTECT(2);
	return ans;
}

