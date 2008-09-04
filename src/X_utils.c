#include "IRanges.h"


static int debug = 0;

SEXP debug_X_utils()
{
#ifdef DEBUG_IRANGES
	debug = !debug;
	Rprintf("Debug mode turned %s in 'X_utils.c'\n", debug ? "on" : "off");
#else
	Rprintf("Debug mode not available in 'X_utils.c'\n");
#endif
	return R_NilValue;
}


/*
 * From R:
 *   .Call("IRanges_sexp_address", 6:4, PACKAGE="IRanges")
 *   .Call("IRanges_sexp_address", new("externalptr"), PACKAGE="IRanges")
 */
SEXP IRanges_sexp_address(SEXP s)
{
	SEXP ans;
	char buf[40]; /* should be enough, even for 128-bit addresses */

	snprintf(buf, sizeof(buf), "%p", s);
	PROTECT(ans = NEW_CHARACTER(1));
	SET_STRING_ELT(ans, 0, mkChar(buf));
	UNPROTECT(1);
	return ans;
}

/*
 * Print some obscure info about an "externalptr" object.
 * From R:
 *   .Call("IRanges_xp_show", new("externalptr"), PACKAGE="IRanges")
 */
SEXP IRanges_xp_show(SEXP xp)
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
 *   xp <- .Call("IRanges_xp_new", PACKAGE="IRanges")
 */
SEXP IRanges_xp_new()
{
	return R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);
}

