#include "IRanges.h"


/* --- .Call ENTRY POINT --- */
SEXP Ranges_order(SEXP start, SEXP width, SEXP decreasing)
{
	int ans_length;
	SEXP ans;

	ans_length = LENGTH(start);
	PROTECT(ans = NEW_INTEGER(ans_length));
	_get_order_of_two_int_arrays(INTEGER(start), INTEGER(width),
		ans_length, LOGICAL(decreasing)[0], INTEGER(ans), 1);
	UNPROTECT(1);
	return ans;
}

