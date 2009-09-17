/****************************************************************************
 *              Low-level manipulation of SharedVector objects              *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_SharedVector_class()
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
 * Some .Call entry points for manipulation externalptr objects.
 */

/*
 * From R:
 *   .Call("externalptr_tagtype", new("externalptr"), PACKAGE="IRanges")
 */
SEXP externalptr_tagtype(SEXP x)
{
	return ScalarString(type2str(TYPEOF(R_ExternalPtrTag(x))));
}

/*
 * Print some info about an externalptr object.
 * From R:
 *   .Call("externalptr_show", new("externalptr"), PACKAGE="IRanges")
 */
SEXP externalptr_show(SEXP x)
{
	void *addr;
	SEXP s;

	Rprintf("Object of class 'externalptr':\n");
	Rprintf("  x adress: %p\n", x);
	addr = R_ExternalPtrAddr(x);
	Rprintf("  R_ExternalPtrAddr(x): %p\n", addr);
	s = R_ExternalPtrTag(x);
	Rprintf("  R_ExternalPtrTag(x): %p\n", s);
	Rprintf("  typeof(R_ExternalPtrTag(x)): %s\n",
				CHAR(type2str(TYPEOF(s))));
	s = R_ExternalPtrProtected(x);
	Rprintf("  R_ExternalPtrProtected(x): %p\n", s);
	Rprintf("  typeof(R_ExternalPtrProtected(x)): %s\n",
				CHAR(type2str(TYPEOF(s))));
	return R_NilValue;
}

/*
 * new("externalptr") will always return the same instance of an external
 * pointer object! If you need a new instance, use this function instead.
 * From R:
 *   x <- .Call("externalptr_new", PACKAGE="IRanges")
 */
SEXP externalptr_new()
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
SEXP _get_SharedVector_tag(SEXP x)
{
	INIT_STATIC_SYMBOL(xp)
	return R_ExternalPtrTag(GET_SLOT(x, xp_symbol));
}

int _get_SharedVector_length(SEXP x)
{
	return LENGTH(_get_SharedVector_tag(x));
}


SEXP SharedVector_length(SEXP x)
{
	return ScalarInteger(_get_SharedVector_length(x));
}


/****************************************************************************
 * C-level setters.
 *
 * Be careful that these functions do NOT duplicate the assigned value!
 */

static void set_SharedVector_tag(SEXP x, SEXP value)
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

/* 'classname' can be "SharedRaw", "SharedInteger" or "SharedDouble" */
SEXP _new_SharedVector(const char *classname, SEXP tag)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));
	set_SharedVector_tag(ans, tag);
	UNPROTECT(2);
	return ans;
}

