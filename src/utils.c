#include "IRanges.h"


/*
 * --- .Call ENTRY POINT ---
 * A C implementation of sapply(list, length)
 * Note: maybe the right place for this is in Biobase...
 */
SEXP sapply_length(SEXP list)
{
	int n = LENGTH(list);
	SEXP ans;
	PROTECT(ans = NEW_INTEGER(n));
	for (int i = 0; i < n; i++) {
		INTEGER(ans)[i] = LENGTH(VECTOR_ELT(list, i));
	}
	UNPROTECT(1);
	return ans;
}

