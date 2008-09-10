#include "IRanges.h"


static int debug = 0;

SEXP debug_ExternalPtr_utils()
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


/*
 * The remaining functions are NOT .Call() entry points!
 * 
 * Do NOT make _new_VectorPtr() or _get_VectorPtr_tag() .Call() entry point!
 * Their argument is NOT duplicated so it would be a disaster if it was
 * coming from the user space.
 */

/* class can be "RawPtr", "IntegerPtr" or "NumericPtr" */
SEXP _new_VectorPtr(const char *class, SEXP tag)
{
        SEXP class_def, ans;

	class_def = MAKE_CLASS(class);
        PROTECT(ans = NEW_OBJECT(class_def));
        SET_SLOT(ans, mkChar("xp"), R_MakeExternalPtr(NULL, tag, R_NilValue));
        UNPROTECT(1);
        return ans;
}

SEXP _get_VectorPtr_tag(SEXP x)
{
        return R_ExternalPtrTag(GET_SLOT(x, install("xp")));
}

int _get_VectorPtr_length(SEXP x)
{
        return LENGTH(_get_VectorPtr_tag(x));
}

