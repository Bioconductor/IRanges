#include "IRanges.h"

static int debug = 0;

SEXP debug_NumericPtr_utils()
{
#ifdef DEBUG_IRANGES
	debug = !debug;
	Rprintf("Debug mode turned %s in 'NumericPtr_utils.c'\n", debug ? "on" : "off");
#else
	Rprintf("Debug mode not available in 'NumericPtr_utils.c'\n");
#endif
	return R_NilValue;
}


/****************************************************************************
 * Memory allocation for a "NumericPtr" object
 * -----------------------------------------
 * An "NumericPtr" object stores its data in an "external" numeric vector
 * (REALSXP vector).
 */

SEXP NumericPtr_alloc(SEXP numptr_xp, SEXP length)
{
	SEXP tag;
	int tag_length;

	tag_length = INTEGER(length)[0];

	PROTECT(tag = NEW_NUMERIC(tag_length));
	R_SetExternalPtrTag(numptr_xp, tag);
	UNPROTECT(1);
	return numptr_xp;
}

/*
 * Return the single string printed by the show method for "NumericPtr" objects.
 * 'numptr_xp' must be the 'xp' slot of a "NumericPtr" object.
 * From R:
 *   numptr <- NumericPtr(30)
 *   .Call("NumericPtr_get_show_string", numptr@xp, PACKAGE="IRanges")
 */
SEXP NumericPtr_get_show_string(SEXP numptr_xp)
{
	SEXP tag, ans;
	int tag_length;
	char buf[100]; /* should be enough... */

	tag = R_ExternalPtrTag(numptr_xp);
	tag_length = LENGTH(tag);
	snprintf(buf, sizeof(buf), "%d-number NumericPtr object (data starting at memory address %p)",
		tag_length, REAL(tag));
	PROTECT(ans = NEW_CHARACTER(1));
	SET_STRING_ELT(ans, 0, mkChar(buf));
	UNPROTECT(1);
	return ans;
}

SEXP NumericPtr_length(SEXP numptr_xp)
{
	SEXP tag, ans;
	int tag_length;

	tag = R_ExternalPtrTag(numptr_xp);
	tag_length = LENGTH(tag);

	PROTECT(ans = NEW_INTEGER(1));
	INTEGER(ans)[0] = tag_length;
	UNPROTECT(1);
	return ans;
}

/*
 * From R:
 *   xn <- NumericPtr(30)
 *   .Call("NumericPtr_memcmp", xn@xp, 1:1, xn@xp, 10:10, 21:21, PACKAGE="IRanges")
 */
SEXP NumericPtr_memcmp(SEXP numptr1_xp, SEXP start1,
		 SEXP numptr2_xp, SEXP start2, SEXP width)
{
	SEXP tag1, tag2, ans;
	int i1, i2, n;

	tag1 = R_ExternalPtrTag(numptr1_xp);
	i1 = INTEGER(start1)[0] - 1;
	tag2 = R_ExternalPtrTag(numptr2_xp);
	i2 = INTEGER(start2)[0] - 1;
	n = INTEGER(width)[0];

	PROTECT(ans = NEW_NUMERIC(1));
	REAL(ans)[0] = _IRanges_memcmp((char *) REAL(tag1), i1,
					(char *) REAL(tag2), i2,
					n, sizeof(double));
	UNPROTECT(1);
	return ans;
}

/* ==========================================================================
 * Read/write numerics to a "NumericPtr" object
 * --------------------------------------------------------------------------
 */

SEXP NumericPtr_read_nums_from_i1i2(SEXP src_numptr_xp, SEXP imin, SEXP imax)
{
	SEXP src, ans;
	int i1, i2, n;

	src = R_ExternalPtrTag(src_numptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	n = i2 - i1 + 1;

	PROTECT(ans = NEW_NUMERIC(n));
	_IRanges_memcpy_from_i1i2(i1, i2,
			(char *) REAL(ans), LENGTH(ans),
			(char *) REAL(src), LENGTH(src), sizeof(double));
	UNPROTECT(1);
	return ans;
}

SEXP NumericPtr_read_nums_from_subset(SEXP src_numptr_xp, SEXP subset)
{
	SEXP src, ans;
	int n;

	src = R_ExternalPtrTag(src_numptr_xp);
	n = LENGTH(subset);
	PROTECT(ans = NEW_NUMERIC(n));
	_IRanges_memcpy_from_subset(INTEGER(subset), n,
			(char *) REAL(ans), n,
			(char *) REAL(src), LENGTH(src), sizeof(double));
	UNPROTECT(1);
	return ans;
}

/*
 * 'val' must be a numeric vector.
 */
SEXP NumericPtr_write_nums_to_i1i2(SEXP dest_numptr_xp, SEXP imin, SEXP imax, SEXP val)
{
	SEXP dest;
	int i1, i2;

	dest = R_ExternalPtrTag(dest_numptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	_IRanges_memcpy_to_i1i2(i1, i2,
			(char *) REAL(dest), LENGTH(dest),
			(char *) REAL(val), LENGTH(val), sizeof(double));
	return dest_numptr_xp;
}

SEXP NumericPtr_write_nums_to_subset(SEXP dest_numptr_xp, SEXP subset, SEXP val)
{
	SEXP dest;

	dest = R_ExternalPtrTag(dest_numptr_xp);
	_IRanges_memcpy_to_subset(INTEGER(subset), LENGTH(subset),
			(char *) REAL(dest), LENGTH(dest),
			(char *) REAL(val), LENGTH(val), sizeof(double));
	return dest_numptr_xp;
}
