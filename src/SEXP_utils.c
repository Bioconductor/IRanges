#include "IRanges.h"


const char *_get_classname(SEXP x)
{
	return CHAR(STRING_ELT(GET_CLASS(x), 0));
}

/*
 * --- .Call ENTRY POINT ---
 * From R:
 *   .Call("address_asSTRSXP", 6:4, PACKAGE="IRanges")
 *   .Call("address_asSTRSXP", new("externalptr"), PACKAGE="IRanges")
 */
SEXP address_asSTRSXP(SEXP s)
{
	char buf[40]; /* should be enough, even for 128-bit addresses */

	snprintf(buf, sizeof(buf), "%p", s);
	return mkString(buf);
}

/*
 * --- .Call ENTRY POINT ---
 * A fast implementation of 'sapply(x, length)' that works only on a list of
 * vectors (or NULLs).
 */
SEXP listofvectors_lengths(SEXP x)
{
	SEXP ans, x_elt;
	int n = LENGTH(x);

	PROTECT(ans = NEW_INTEGER(n));
	for (int i = 0; i < n; i++) {
		x_elt = VECTOR_ELT(x, i);
		if (x_elt == R_NilValue) {
			INTEGER(ans)[i] = 0;
			continue;
		}
		if (!IS_VECTOR(x_elt))
			error("element %d not a vector (or NULL)", i + 1);
		INTEGER(ans)[i] = LENGTH(x_elt);
	}
	UNPROTECT(1);
	return ans;
}

