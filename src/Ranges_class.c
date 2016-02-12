/****************************************************************************
 *                 Low-level manipulation of Ranges objects                 *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"


/*
 * --- .Call ENTRY POINT ---
 * Doesn't raise an error but returns NULL or a single string describing the
 * first encountered validity failure.
 */
SEXP valid_Ranges(SEXP x_start, SEXP x_end, SEXP x_width)
{
	static char validity_failures[200];

	int x_len, i, tmp;
	const int *x_start_p, *x_end_p, *x_width_p;

	if (!IS_INTEGER(x_start) ||
	    !IS_INTEGER(x_end) ||
	    !IS_INTEGER(x_width))
	{
		snprintf(validity_failures, sizeof(validity_failures),
			 "'%s', '%s', and '%s' must be integer vectors",
			 "start(x)", "end(x)", "width(x)");
		goto failure;
	}
	x_len = LENGTH(x_start);
	if (LENGTH(x_end) != x_len ||
	    LENGTH(x_width) != x_len)
	{
		snprintf(validity_failures, sizeof(validity_failures),
			 "'%s', '%s', and '%s' must have the same length",
			 "start(x)", "end(x)", "width(x)");
		goto failure;
	}
	for (i = 0, x_start_p = INTEGER(x_start),
		    x_end_p = INTEGER(x_end),
		    x_width_p = INTEGER(x_width);
	     i < x_len;
	     i++, x_start_p++, x_end_p++, x_width_p++)
	{
		if (*x_start_p == NA_INTEGER ||
		    *x_end_p == NA_INTEGER ||
		    *x_width_p == NA_INTEGER)
		{
			snprintf(validity_failures, sizeof(validity_failures),
				 "'%s', '%s', and '%s' cannot contain NAs",
				 "start(x)", "end(x)", "width(x)");
			goto failure;
		}
		if (*x_width_p < 0) {
			snprintf(validity_failures, sizeof(validity_failures),
				 "'%s' cannot contain negative integers",
				 "width(x)");
			goto failure;
		}
		/* Safe because NA_INTEGER == INT_MIN (see R_ext/Arith.h) */
		tmp = *x_start_p - 1;
		/* The purpose of the 1st part of the test (the part before ||)
		   is to avoid an integer overflow during the 2nd part of the
		   test (the part after ||). */
		if (tmp > INT_MAX - *x_width_p ||
		    tmp + *x_width_p != *x_end_p)
		{
			snprintf(validity_failures, sizeof(validity_failures),
				 "'%s[i] - %s[i] != %s[i] + 1' for i = %d",
				 "end(x)", "start(x)", "width(x)", i + 1);
			goto failure;
		}
	}
	return R_NilValue;
failure:
	return mkString(validity_failures);
}

